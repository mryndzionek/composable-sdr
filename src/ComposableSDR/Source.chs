module ComposableSDR.Source
  ( openSource
  , readChunks
  , readBytes
  , closeSource
  , enumerate
  , readFromFile
  ) where

#include <SoapySDR/Device.h>

import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array                      hiding (newArray)

import           GHC.ForeignPtr                             (mallocPlainForeignPtrBytes)

import           Data.List                                  (foldl')

import           Control.Exception                          (throwIO)
import           Control.Monad
import qualified Control.Monad.Catch                        as MC

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import           Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import qualified Streamly.Internal.FileSystem.File          as FS
import qualified Streamly.Memory.Array                      as A

import qualified Streamly.Internal.Memory.Array.Types       as AT (Array (..),
                                                                   shrinkToFit)
import qualified Streamly.Internal.Memory.ArrayStream       as AS

import           ComposableSDR.Common

data SoapySource = SoapySource
  { _dev      :: Ptr SoapySDRDevice
  , _stream   :: Ptr SoapySDRStream
  }

{#pointer *SoapySDRKwargs as SoapySDRKwargs#}

_get :: Ptr b -> (Ptr b -> IO (Ptr CString)) -> IO [String]
_get args getter = do
  s <- fromIntegral <$> getSize args
  k <- getter args
  traverse (peekElemOff k >=> peekCString) [0 .. s - 1]

getKeys, getValues :: Ptr b -> IO (Ptr (Ptr CChar))
getKeys = {#get SoapySDRKwargs->keys#}
getValues = {#get SoapySDRKwargs->vals#}

setKeys, setValues :: Ptr b -> Ptr (Ptr CChar) -> IO ()
setKeys = {#set SoapySDRKwargs->keys#}
setValues = {#set SoapySDRKwargs->vals#}

setSize :: Ptr b -> CULong -> IO ()
setSize = {#set SoapySDRKwargs->size#}

getSize :: Ptr b -> IO CULong
getSize = {#get SoapySDRKwargs->size#}

allocArgs :: [(String, String)] -> IO (Ptr SoapySDRKwargs)
allocArgs a = do
  as <- calloc
  let s = length a
  ks <- mallocArray s
  vs <- mallocArray s
  kp <- mapM (newCString . fst) a
  vp <- mapM (newCString . snd) a
  pokeArray ks kp
  pokeArray vs vp
  setKeys as ks
  setValues as vs
  setSize as $ fromIntegral s
  return as

freeArgs :: Ptr SoapySDRKwargs -> IO ()
freeArgs pa = do
  s <- fromIntegral <$> getSize pa
  ks <- getKeys pa
  vs <- getValues pa
  pk <- peekArray s ks
  pv <- peekArray s vs
  mapM_ free pk
  mapM_ free pv
  free ks
  free vs
  free pa

foreign import ccall unsafe "SoapySDRDevice_enumerate"
    c_SoapySDRDevice_enumerate :: Ptr SoapySDRKwargs -> Ptr CSize -> IO (Ptr SoapySDRKwargs)

foreign import ccall unsafe "SoapySDRKwargsList_clear"
    c_SoapySDRKwargsList_clear :: Ptr SoapySDRKwargs -> CSize -> IO ()

enumerate :: IO [[(String, String)]]
enumerate =
  alloca $ \s -> do
    ptr <- c_SoapySDRDevice_enumerate nullPtr s
    sz <- peek s
    let as = foldl' (\b a -> advancePtr ptr a : b) [] [0 .. fromIntegral sz - 1]
    let ex b = do
          ks <- _get b getKeys
          vs <- _get b getValues
          return $ zip ks vs
    res <- mapM ex as
    c_SoapySDRKwargsList_clear ptr sz
    return res

{#pointer *SoapySDRDevice as SoapySDRDevice#}

foreign import ccall unsafe "SoapySDRDevice_make" c_SoapySDRDevice_make
  :: Ptr SoapySDRKwargs -> IO (Ptr SoapySDRDevice)

foreign import ccall unsafe "SoapySDRDevice_unmake" c_SoapySDRDevice_unmake
  :: Ptr SoapySDRDevice -> IO CInt

soapySdrRx :: CInt
soapySdrRx = 1

foreign import ccall unsafe "SoapySDRDevice_setSampleRate" c_SoapySDRDevice_setSampleRate
  :: Ptr SoapySDRDevice -> CInt -> CSize -> CDouble -> IO CInt

foreign import ccall unsafe "SoapySDRDevice_setFrequency" c_SoapySDRDevice_setFrequency
  :: Ptr SoapySDRDevice ->
  CInt -> CSize -> CDouble -> Ptr SoapySDRKwargs -> IO CInt

foreign import ccall unsafe "SoapySDRDevice_setGain" c_SoapySDRDevice_setGain
  :: Ptr SoapySDRDevice ->
  CInt -> CSize -> CDouble -> IO CString

foreign import ccall unsafe "SoapySDRDevice_setGainMode" c_SoapySDRDevice_setGainMode
  :: Ptr SoapySDRDevice ->
  CInt -> CSize -> CUChar -> IO CInt

createSoapyDevice ::
     String -> Double -> Double -> Double -> IO (Ptr SoapySDRDevice)
createSoapyDevice dn sr freq gain = do
  args <- allocArgs [("driver", dn)]
  dev <- c_SoapySDRDevice_make args
  freeArgs args
  try (c_SoapySDRDevice_setSampleRate dev soapySdrRx 0 (CDouble sr))
  try (c_SoapySDRDevice_setFrequency dev soapySdrRx 0 (CDouble freq) nullPtr)
  if gain == 0.0
    then try (c_SoapySDRDevice_setGainMode dev soapySdrRx 0 1)
    else do
      s <- c_SoapySDRDevice_setGain dev soapySdrRx 0 (CDouble gain)
      if s == nullPtr
        then return ()
        else throwIO SoapyException
  return dev

{#pointer *SoapySDRStream as SoapySDRStream#}

foreign import ccall unsafe "SoapySDRDevice_setupStream" c_SoapySDRDevice_setupStream
  :: Ptr SoapySDRDevice ->
  CInt ->
    CString ->
      Ptr CSize -> CSize -> Ptr SoapySDRKwargs -> IO (Ptr SoapySDRStream)

foreign import ccall unsafe "SoapySDRDevice_activateStream" c_SoapySDRDevice_activateStream
  :: Ptr SoapySDRDevice ->
  Ptr SoapySDRStream -> CInt -> CLLong -> CSize -> IO CInt

foreign import ccall unsafe "SoapySDRDevice_deactivateStream" c_SoapySDRDevice_deactivateStream
  :: Ptr SoapySDRDevice ->
  Ptr SoapySDRStream -> CInt -> CLLong -> IO CInt

foreign import ccall unsafe "SoapySDRDevice_readStream" c_SoapySDRDevice_readStream
  :: Ptr SoapySDRDevice ->
  Ptr SoapySDRStream ->
    Ptr (Ptr ()) -> CSize -> Ptr CInt -> Ptr CLLong -> CLong -> IO CInt

foreign import ccall unsafe "SoapySDRDevice_closeStream" c_SoapySDRDevice_closeStream
  :: Ptr SoapySDRDevice -> Ptr SoapySDRStream -> IO CInt

foreign import ccall unsafe "SoapySDRDevice_getStreamMTU" c_SoapySDRDevice_getStreamMTU
  :: Ptr SoapySDRDevice -> Ptr SoapySDRStream -> IO CSize

openSource :: String -> Double -> Double -> Double -> IO SoapySource
openSource dn sr freq gain = do
  dev <- createSoapyDevice dn sr freq gain
  fmt <- newCString sampleFormat
  args <- allocArgs [("buffers", "30")]
  stream <- c_SoapySDRDevice_setupStream dev soapySdrRx fmt nullPtr 0 args
  freeArgs args
  when (stream == nullPtr) $ throwIO SoapyException
  free fmt
  try (c_SoapySDRDevice_activateStream dev stream 0 0 0)
  return $ SoapySource dev stream

{-# INLINABLE _read #-}
_read :: (MonadIO m) => SoapySource -> m (AT.Array SamplesIQCF32)
_read src =
  liftIO $ do
    numsamples <-
      fromIntegral <$> c_SoapySDRDevice_getStreamMTU (_dev src) (_stream src)
    ptr <- mallocPlainForeignPtrBytes (elemSize * numsamples)
    withForeignPtr ptr $ \p ->
      alloca $ \flags ->
        alloca $ \timeNs ->
          alloca $ \bp -> do
            poke bp p
            s <-
              c_SoapySDRDevice_readStream
                (_dev src)
                (_stream src)
                (castPtr bp)
                (fromIntegral numsamples)
                flags
                timeNs
                1000000
            let s' = max 0 s
                v =
                  AT.Array
                    { AT.aStart = ptr
                    , AT.aEnd = p `plusPtr` (elemSize * numsamples)
                    , AT.aBound = p `plusPtr` (elemSize * fromIntegral s')
                    }
            AT.shrinkToFit v

{-# INLINE [1] readChunks #-}
readChunks ::
     (MonadIO m, IsStream t) => SoapySource -> t m (AT.Array SamplesIQCF32)
readChunks s = D.fromStreamD (D.Stream step ())
  where
    {-# INLINE [0] step #-}
    step _ _ = do
      arr <- _read s
      return $
        case A.length arr of
          0 -> D.Stop
          _ -> D.Yield arr ()

{-# INLINE readBytes #-}
readBytes :: (MonadIO m, IsStream t) => SoapySource -> t m SamplesIQCF32
readBytes s = AS.concat $ readChunks s

closeSource :: SoapySource -> IO ()
closeSource s = do
  let (dev, stream) = (_dev s, _stream s)
  try (c_SoapySDRDevice_deactivateStream dev stream 0 0)
  try (c_SoapySDRDevice_closeStream dev stream)
  try (c_SoapySDRDevice_unmake dev)

readFromFile ::
     (IsStream t, MC.MonadCatch m, MonadIO m, Monad (t m))
  => Int
  -> FilePath
  -> t m (A.Array SamplesIQCF32)
readFromFile n fp =
  let adapt a =
        AT.Array
          { AT.aStart = castForeignPtr $ AT.aStart a
          , AT.aEnd = castPtr $ AT.aEnd a
          , AT.aBound = castPtr $ AT.aBound a
          }
   in adapt <$> FS.toChunksWithBufferOf (elemSize * n) fp