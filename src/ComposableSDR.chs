{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ComposableSDR
  ( enumerate
  , fileSink
  , stdOutSink
  , audioFileSink
  , openSource
  , readChunks
  , readBytes
  , closeSource
  , elemSize
  , resamplerCreate
  , resample
  , resamplerDestroy
  , fmDemodulator
  , wbFMDemodulator
  , amDemodulator
  , firDecimator
  , automaticGainControl
  , iirFilter
  , dcBlocker
  , firpfbchChannelizer
  , realToComplex
  , complexToReal
  , takeNArr
  , foldArray
  , distribute_
  , mix
  , AudioFormat(..)
  , SamplesIQCF32
  ) where

#include <SoapySDR/Device.h>
#include <SoapySDR/Formats.h>
#include <liquid/liquid.h>

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array                      hiding (newArray)
import           Foreign.Ptr
import           Foreign.Storable

import           Control.Exception                          (Exception, throwIO)
import           Control.Monad
import qualified Control.Monad.Catch                        as MC
import           Control.Monad.State
import           Data.Complex
import           Data.List                                  (foldl', unfoldr)
import           Data.Typeable

import           Foreign.ForeignPtr                         (plusForeignPtr,
                                                             withForeignPtr)
import           GHC.ForeignPtr                             (mallocPlainForeignPtrBytes)

import           Foreign.ForeignPtr.Unsafe                  (unsafeForeignPtrToPtr)

import           System.IO                                  (stdout)

import qualified Streamly.Prelude                           as S

import qualified Sound.File.Sndfile                         as SF

import qualified Streamly.Internal.Data.Fold                as F
import qualified Streamly.Internal.Data.Fold.Types          as FL
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import           Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import qualified Streamly.Internal.FileSystem.File          as FS
import qualified Streamly.Internal.FileSystem.Handle        as FH
import qualified Streamly.Internal.Memory.Array.Types       as AT (Array (..),
                                                                   newArray,
                                                                   newArray,
                                                                   shrinkToFit,
                                                                   spliceTwo,
                                                                   splitAt)
import qualified Streamly.Internal.Memory.ArrayStream       as AS
import qualified Streamly.Memory.Array                      as A

data SoapyException =
  SoapyException
  deriving (Show, Typeable)

instance Exception SoapyException

newtype Array a = Array
  { fromArray :: AT.Array a
  }

toArray :: AT.Array a -> Array a
toArray = Array

instance Storable a => SF.Buffer Array a where
    fromForeignPtr fp i n =
      withForeignPtr fp $ \p -> do
        let v =
              AT.Array
                { AT.aStart = fp `plusForeignPtr` i
                , AT.aEnd = p `plusPtr` (i + n)
                , AT.aBound = p `plusPtr` (i + n)
                }
        return $ toArray v

    toForeignPtr = return . (\x -> (AT.aStart x, 0, A.length x)) . fromArray

data AudioFormat
  = AU
  | WAV
  deriving (Show, Read)

type Processor m a b c = FL.Fold m b c -> FL.Fold m a c
type ArrayProcessor m a b c = Processor m (A.Array a) (A.Array b) c
type Demodulator m c = ArrayProcessor m SamplesIQCF32 Float c

data SoapySource = SoapySource
  { _dev      :: Ptr SoapySDRDevice
  , _stream   :: Ptr SoapySDRStream
  }

elemSize :: Int
elemSize = 8

sampleFormat :: String
sampleFormat = "CF32"

type SamplesIQCF32 = Complex CFloat

try :: (MonadIO m, Num a, Ord a) => m a -> m ()
try a = a >>= (\s -> when (s < 0) $ liftIO $ throwIO SoapyException)

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

takeNArr ::
     (IsStream t, Storable a, MonadIO m)
  => Int
  -> t m (AT.Array a)
  -> t m (AT.Array a)
takeNArr n = S.map fst . S.takeWhile ((/= 0) . snd) . foldArrL
  where
    foldArrL = S.postscan fld
    fld =
      FL.Fold
        (\(_, l) a -> return $ update a l)
        ((,) <$> liftIO (AT.newArray 0) <*> return 0)
        return
    update a l =
      let togo = n - l
       in if togo == 0
            then (a, 0)
            else if togo >= A.length a
                   then (a, l + A.length a)
                   else (trim a togo, n)
    trim a l =
      if l < A.length a
        then fst $ AT.splitAt l a
        else a

splitterBy :: Storable a => Int -> ArrayProcessor IO a a b
splitterBy n (FL.Fold step1 start1 done1) = FL.Fold step start done
  where
    start = do
      s <- start1
      a0 <- AT.newArray 0
      return (s, a0)
    done (s, b) = do
      s' <- step1 s b
      done1 s'
    step (s, b) a =
      let m = A.length b + A.length a
       in if m >= n
            then do
              ba <- AT.spliceTwo b a
              (a', b') <-
                if m == n
                  then do
                    a0 <- AT.newArray 0
                    return (ba, a0)
                  else return $ AT.splitAt n ba
              s' <- step1 s a'
              return (s', b')
            else do
              ba <- AT.spliceTwo b a
              return (s, ba)

{-# INLINABLE fileSink #-}
fileSink :: (MonadIO m, MC.MonadCatch m, Storable a) => FilePath -> FL.Fold m (A.Array a) ()
fileSink = FS.writeChunks

{-# INLINABLE stdOutSink #-}
stdOutSink :: (MonadIO m, MC.MonadCatch m, Storable a) => FL.Fold m (A.Array a) ()
stdOutSink = FH.writeChunks stdout

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

{#pointer msresamp_crcf as MsResampCrcf#}

foreign import ccall unsafe "msresamp_crcf_create" c_msresamp_crcf_create
  :: CFloat -> CFloat -> IO MsResampCrcf

foreign import ccall unsafe "msresamp_crcf_print" c_msresamp_crcf_print
  :: MsResampCrcf -> IO ()

foreign import ccall unsafe "msresamp_crcf_get_rate" c_msresamp_crcf_get_rate
  :: MsResampCrcf -> IO CFloat

foreign import ccall unsafe "msresamp_crcf_execute" c_msresamp_crcf_execute
  :: MsResampCrcf ->
  Ptr SamplesIQCF32 ->
    CUInt -> Ptr SamplesIQCF32 -> Ptr CUInt -> IO ()

foreign import ccall unsafe "msresamp_crcf_destroy" c_msresamp_crcf_destroy
  :: MsResampCrcf -> IO ()

resample :: (MonadIO m) => MsResampCrcf -> AT.Array SamplesIQCF32 -> m (AT.Array SamplesIQCF32)
resample rs x =
  liftIO $ do
    r <- c_msresamp_crcf_get_rate rs
    let nx = fromIntegral $ A.length x
        ny = 2 * ceiling (r * fromIntegral nx)
    fy <- mallocPlainForeignPtrBytes (elemSize * ny)
    withForeignPtr fy $ \y ->
      alloca $ \pnwy -> do
        c_msresamp_crcf_execute
          rs
          (castPtr . unsafeForeignPtrToPtr $ AT.aStart x)
          nx
          y
          pnwy
        nwy <- fromIntegral <$> peek pnwy
        let v =
              AT.Array
                { AT.aStart = fy
                , AT.aEnd = y `plusPtr` (elemSize * nwy)
                , AT.aBound = y `plusPtr` (elemSize * ny)
                }
        AT.shrinkToFit v

resamplerCreate :: Float -> Float -> IO MsResampCrcf
resamplerCreate r as =
  case r of
    0 -> return nullPtr
    _ -> do
      res <- c_msresamp_crcf_create (CFloat r) (CFloat as)
      putStrLn "Using resampler:"
      c_msresamp_crcf_print res
      return res

resamplerDestroy :: MsResampCrcf -> IO ()
resamplerDestroy r
  | r == nullPtr = return ()
  | otherwise = c_msresamp_crcf_destroy r

toProcessor ::
     MonadIO m
  => m r
  -> (r -> m ())
  -> (r -> a -> m b)
  -> FL.Fold m b c
  -> FL.Fold m a c
toProcessor bef aft bet (FL.Fold step1 start1 done1) = FL.Fold step start done
  where
    start = do
      s <- start1
      r <- bef
      return (s, r, bet r)
    done (s, r, _) = do
      aft r
      done1 s
    step (s, r, f) a = do
      b <- f a
      s' <- step1 s b
      return (s', r, f)

{#pointer freqdem as FreqDem#}

foreign import ccall unsafe "freqdem_create" c_freqdem_create
  :: CFloat -> IO FreqDem

foreign import ccall unsafe "freqdem_print" c_freqdem_print
  :: FreqDem -> IO ()

foreign import ccall unsafe "freqdem_demodulate_block" c_freqdem_demodulate_block
  :: FreqDem -> Ptr SamplesIQCF32 -> CUInt -> Ptr Float -> IO ()

foreign import ccall unsafe "freqdem_destroy" c_freqdem_crcf_destroy
  :: FreqDem -> IO ()

fmdemodCreate :: Float -> IO FreqDem
fmdemodCreate kf = do
  d <- c_freqdem_create (CFloat kf)
  putStrLn "Using FM demodulator:"
  c_freqdem_print d
  return d

wrap ::
     (MonadIO m, Storable a, Storable b)
  => Int -> Int
  -> (Ptr a -> CUInt -> Ptr b -> IO ())
  -> A.Array a
  -> m (A.Array b)
wrap ny nyb cdem x =
  liftIO $ do
    fy <- mallocPlainForeignPtrBytes nyb
    withForeignPtr fy $ \y -> do
      cdem (castPtr . unsafeForeignPtrToPtr $ AT.aStart x) (fromIntegral ny) y
      let v =
            AT.Array
              { AT.aStart = fy
              , AT.aEnd = y `plusPtr` nyb
              , AT.aBound = y `plusPtr` nyb
              }
      AT.shrinkToFit v

fmDemod ::
     (MonadIO m) => FreqDem -> AT.Array SamplesIQCF32 -> m (AT.Array Float)
fmDemod d a =
  let n = fromIntegral $ A.length a
   in wrap n (4 * n) (c_freqdem_demodulate_block d) a

fmdemodDestroy :: FreqDem -> IO ()
fmdemodDestroy = c_freqdem_crcf_destroy

fmDemodulator :: Float -> Demodulator IO a
fmDemodulator kf = toProcessor (fmdemodCreate kf) fmdemodDestroy fmDemod

{#pointer ampmodem as AmpDem#}

foreign import ccall unsafe "ampmodem_create" c_ampmodem_create
  :: CFloat -> CInt -> CInt -> IO AmpDem

foreign import ccall unsafe "ampmodem_print" c_ampmodem_print :: AmpDem -> IO ()

foreign import ccall unsafe "ampmodem_demodulate_block" c_ampmodem_demodulate_block
  :: AmpDem -> Ptr SamplesIQCF32 -> CUInt -> Ptr Float -> IO ()

foreign import ccall unsafe "ampmodem_destroy" c_ampmodem_destroy
  :: AmpDem -> IO ()

amdemodCreate :: IO AmpDem
amdemodCreate = do
  -- TODO expose parameters in typesafe way
  demod <- c_ampmodem_create 0.8 0 0
  putStrLn "Using AM demodulator:"
  c_ampmodem_print demod
  return demod

amDemod :: MonadIO m => AmpDem -> A.Array SamplesIQCF32 -> m (A.Array Float)
amDemod d a =
  let n = fromIntegral $ A.length a
   in wrap n (4 * n) (c_ampmodem_demodulate_block d) a

amdemodDestroy :: AmpDem -> IO ()
amdemodDestroy = c_ampmodem_destroy

amDemodulator :: Demodulator IO a
amDemodulator = toProcessor amdemodCreate amdemodDestroy amDemod

openAudioFile :: AudioFormat -> FilePath -> Int -> Int -> IO SF.Handle
openAudioFile fmt fp sr sn = SF.openFile (fp ++ ext) SF.WriteMode info
  where
    sfFmt AU  = (SF.HeaderFormatAu, ".au")
    sfFmt WAV = (SF.HeaderFormatWav, ".wav")
    (sff, ext) = sfFmt fmt
    info =
      SF.Info sn sr 1 (SF.Format sff SF.SampleFormatFloat SF.EndianBig) 1 False

writeToAudioFile :: (MonadIO m, SF.Sample a) => SF.Handle -> A.Array a -> m ()
writeToAudioFile h a = do
  _ <- liftIO $ SF.hPutBuffer h (toArray a)
  return ()

closeAudioFile :: SF.Handle -> IO ()
closeAudioFile = SF.hClose

audioFileSink ::
     (MC.MonadCatch m, MonadIO m, SF.Sample a)
  => AudioFormat
  -> Int
  -> Int
  -> String
  -> FL.Fold m (A.Array a) ()
audioFileSink fmt sr sn fp =
  let wav = openAudioFile fmt fp sr sn
   in bracketIO wav closeAudioFile (F.drainBy . writeToAudioFile)

foldArray :: Storable a => (A.Array a -> IO b) -> FL.Fold IO (A.Array a) b
foldArray = FL.Fold AT.spliceTwo (AT.newArray 0)

{-# INLINE bracketIO #-}
bracketIO ::
     (MC.MonadCatch m, MonadIO m)
  => IO r
  -> (r -> IO c)
  -> (r -> FL.Fold m a b)
  -> FL.Fold m a b
bracketIO bef aft bet = FL.Fold step initial extract
  where
    initial = do
      res <- liftIO bef
      return (bet res, res)
    step (fld', res) x = do
      r <- FL.runStep fld' x `MC.onException` liftIO (aft res)
      return (r, res)
    extract (FL.Fold _ initial1 extract1, res) = do
      _ <- liftIO $ aft res
      initial1 >>= extract1

{#pointer firdecim_rrrf as FirDecim#}

foreign import ccall unsafe "firdecim_rrrf_create_kaiser" c_firdecim_rrrf_create_kaiser
  :: CUInt -> CUInt -> CFloat -> IO FirDecim

foreign import ccall unsafe "firdecim_rrrf_print" c_firdecim_rrrf_print
  :: FirDecim -> IO ()

foreign import ccall unsafe "firdecim_rrrf_execute_block" c_firdecim_rrrf_execute_block
  :: FirDecim -> Ptr Float -> CUInt -> Ptr Float -> IO ()

foreign import ccall unsafe "firdecim_rrrf_destroy" c_firdecim_rrrf_destroy
  :: FirDecim -> IO ()

firdecimCreate :: Int -> IO FirDecim
firdecimCreate m = do
  f <- c_firdecim_rrrf_create_kaiser (fromIntegral m) 5 60.0
  putStrLn "Using output decimator:"
  c_firdecim_rrrf_print f
  return f

firdecimDestroy :: FirDecim -> IO ()
firdecimDestroy = c_firdecim_rrrf_destroy

firDecim :: MonadIO m => Int -> FirDecim -> A.Array Float -> m (A.Array Float)
firDecim m f a =
  let n = fromIntegral (A.length a) `div` m
   in wrap n (4 * n) (c_firdecim_rrrf_execute_block f) a

firDecimator :: Int -> ArrayProcessor IO Float Float a
firDecimator m = toProcessor (firdecimCreate m) firdecimDestroy (firDecim m)

{#pointer firhilbf as FirHilb#}

foreign import ccall unsafe "firhilbf_create" c_firhilbf_create
  :: CUInt -> CFloat -> IO FirHilb

foreign import ccall unsafe "firhilbf_print" c_firhilbf_print
  :: FirHilb -> IO ()

foreign import ccall unsafe "firhilbf_decim_execute_block" c_firhilbf_decim_execute_block
  :: FirHilb -> Ptr Float -> CUInt -> Ptr SamplesIQCF32 -> IO ()

foreign import ccall unsafe "firhilbf_interp_execute_block" c_firhilbf_interp_execute_block
  :: FirHilb -> Ptr SamplesIQCF32 -> CUInt -> Ptr Float -> IO ()

foreign import ccall unsafe "firhilbf_destroy" c_firhilbf_destroy
  :: FirHilb -> IO ()

firhilbCreate :: IO FirHilb
firhilbCreate = do
  f <- c_firhilbf_create 5 60.0
  putStrLn "Using Hilbert transform"
  c_firhilbf_print f
  return f

firhilbDestroy :: FirHilb -> IO ()
firhilbDestroy = c_firhilbf_destroy

firhilbDecim ::
     MonadIO m => FirHilb -> A.Array Float -> m (A.Array SamplesIQCF32)
firhilbDecim f a =
  let n = fromIntegral (A.length a) `div` 2
   in wrap n (n * 8) (c_firhilbf_decim_execute_block f) a

realToComplex :: ArrayProcessor IO Float SamplesIQCF32 a
realToComplex = toProcessor firhilbCreate firhilbDestroy firhilbDecim

firhilbInterp ::
     MonadIO m => FirHilb -> A.Array SamplesIQCF32 -> m (A.Array Float)
firhilbInterp f a =
  let n = (fromIntegral $ A.length a)
   in wrap n (2 * 4 * n) (c_firhilbf_interp_execute_block f) a

complexToReal :: ArrayProcessor IO SamplesIQCF32 Float a
complexToReal = toProcessor firhilbCreate firhilbDestroy firhilbInterp

{#pointer iirfilt_crcf as IirFiltC#}

foreign import ccall unsafe "iirfilt_crcf_create_dc_blocker" c_iirfilt_crcf_create_dc_blocker
  :: CFloat -> IO IirFiltC

foreign import ccall unsafe "iirfilt_crcf_print" c_iirfilt_crcf_print
  :: IirFiltC -> IO ()

foreign import ccall unsafe "iirfilt_crcf_execute_block" c_iirfilt_crcf_execute_block
  :: IirFiltC -> Ptr SamplesIQCF32 -> CUInt -> Ptr SamplesIQCF32 -> IO ()

foreign import ccall unsafe "iirfilt_crcf_destroy" c_iirfilt_crcf_destroy
  :: IirFiltC -> IO ()

{#pointer iirfilt_rrrf as IirFilt#}

foreign import ccall unsafe "iirfilt_rrrf_create" c_iirfilt_rrrf_create
  :: Ptr Float -> CUInt -> Ptr Float -> CUInt -> IO IirFilt

foreign import ccall unsafe "iirfilt_rrrf_print" c_iirfilt_rrrf_print
  :: IirFilt -> IO ()

foreign import ccall unsafe "iirfilt_rrrf_execute_block" c_iirfilt_rrrf_execute_block
  :: IirFilt -> Ptr Float -> CUInt -> Ptr Float -> IO ()

foreign import ccall unsafe "iirfilt_rrrf_destroy" c_iirfilt_rrrf_destroy
  :: IirFilt -> IO ()

iirfiltCreate :: [Float] -> [Float] -> IO IirFilt
iirfiltCreate bt at =
  alloca $ \pbt ->
    alloca $ \pat -> do
      pokeArray pbt bt
      pokeArray pat at
      f <-
        c_iirfilt_rrrf_create
          pbt
          (fromIntegral $ length bt)
          pat
          (fromIntegral $ length at)
      putStrLn "Using IIR filter:"
      c_iirfilt_rrrf_print f
      return f

iirfiltDestroy :: IirFilt -> IO ()
iirfiltDestroy = c_iirfilt_rrrf_destroy

iirFilt ::
     MonadIO m => IirFilt -> A.Array Float -> m (A.Array Float)
iirFilt f a =
  let n = (fromIntegral $ A.length a)
   in wrap n (4 * n) (c_iirfilt_rrrf_execute_block f) a

iirFilter :: [Float] -> [Float] -> ArrayProcessor IO Float Float a
iirFilter bt at = toProcessor (iirfiltCreate bt at) iirfiltDestroy iirFilt

dcBlockerCreate :: IO IirFiltC
dcBlockerCreate = do
  dc <- c_iirfilt_crcf_create_dc_blocker 0.0005
  putStrLn "Using DC blocker:"
  c_iirfilt_crcf_print dc
  return dc

iirCFilt ::
     MonadIO m => IirFiltC -> A.Array SamplesIQCF32 -> m (A.Array SamplesIQCF32)
iirCFilt f a =
  let n = (fromIntegral $ A.length a)
   in wrap n (8 * n) (c_iirfilt_crcf_execute_block f) a

dcBlocker :: ArrayProcessor IO SamplesIQCF32 SamplesIQCF32 a
dcBlocker = toProcessor dcBlockerCreate iirfiltCDestroy iirCFilt

iirfiltCDestroy :: IirFiltC -> IO ()
iirfiltCDestroy = c_iirfilt_crcf_destroy

-- This is mostly taken from GNU Radio
fmDeemphTaps :: Double -> ([Float], [Float])
fmDeemphTaps fs =
  let tau = 75e-6
      wc = 1 / tau
      wca = 2.0 * fs * tan (wc / (2.0 * fs))
      k = -wca / (2.0 * fs)
      z1 = -1.0
      p1 = (1.0 + k) / (1.0 - k)
      b0 = -k / (1.0 - k)
      btaps = realToFrac <$> [b0 * 1.0, b0 * (-z1)]
      ataps = realToFrac <$> [1.0, -p1]
   in (btaps, ataps)

wbFMDemodulator :: Double -> Int -> Demodulator IO a
wbFMDemodulator quadRate decim =
  let (dbt, dat) = fmDeemphTaps (quadRate / fromIntegral decim)
   in fmDemodulator 0.6 . iirFilter dbt dat . firDecimator decim

{#pointer agc_crcf as Agc#}

foreign import ccall unsafe "agc_crcf_create" c_agc_crcf_create
  :: IO Agc

foreign import ccall unsafe "agc_crcf_print" c_agc_crcf_print
  :: Agc -> IO ()

foreign import ccall unsafe "agc_crcf_set_bandwidth" c_agc_crcf_set_bandwidth
  :: Agc -> Float -> IO ()

foreign import ccall unsafe "agc_crcf_set_signal_level" c_agc_crcf_set_signal_level
  :: Agc -> Float -> IO ()

foreign import ccall unsafe "agc_crcf_squelch_enable" c_agc_crcf_squelch_enable
  :: Agc -> IO ()

foreign import ccall unsafe "agc_crcf_squelch_set_threshold" c_agc_crcf_squelch_set_threshold
  :: Agc -> Float -> IO ()

foreign import ccall unsafe "agc_crcf_squelch_set_timeout" c_agc_crcf_squelch_set_timeout
  :: Agc -> CUInt -> IO ()

foreign import ccall unsafe "agc_crcf_execute_block" c_agc_crcf_execute_block
  :: Agc -> Ptr SamplesIQCF32 -> CUInt -> Ptr SamplesIQCF32 -> IO ()

foreign import ccall unsafe "agc_crcf_get_rssi" c_agc_crcf_get_rssi
  :: Agc -> IO Float

foreign import ccall unsafe "agc_crcf_squelch_get_status" c_agc_squelch_crcf_get_status
  :: Agc -> IO CInt

foreign import ccall unsafe "agc_crcf_destroy" c_agc_crcf_destroy
  :: Agc -> IO ()

agcExecuteBlock ::
     Agc -> Ptr SamplesIQCF32 -> CUInt -> Ptr SamplesIQCF32 -> IO ()
agcExecuteBlock agc px n py = do
  let n' :: Int = fromIntegral n
      xsp = advancePtr px <$> [0 .. fromIntegral n' - 1]
      ysp = advancePtr py <$> [0 .. fromIntegral n' - 1]
      go x y = do
        c_agc_crcf_execute_block agc x 1 y
        s <- c_agc_squelch_crcf_get_status agc
        _ <- c_agc_crcf_get_rssi agc
        when (s == 1) (poke y (0 :+ 0)) -- squelch enabled
  zipWithM_ go xsp ysp

agcCreate :: Float -> IO Agc
agcCreate tres = do
  a <- c_agc_crcf_create
  c_agc_crcf_set_bandwidth a 0.25
  c_agc_crcf_set_signal_level a 1e-3
  c_agc_crcf_squelch_enable a
  c_agc_crcf_squelch_set_threshold a tres
  c_agc_crcf_squelch_set_timeout a 100
  putStrLn "Using AGC:"
  c_agc_crcf_print a
  return a

agcDestroy :: Agc -> IO ()
agcDestroy = c_agc_crcf_destroy

agcEx :: MonadIO m => Agc -> A.Array SamplesIQCF32 -> m (A.Array SamplesIQCF32)
agcEx agc a =
  let n = (fromIntegral $ A.length a)
   in wrap n (8 * n) (agcExecuteBlock agc) a

automaticGainControl :: Float -> ArrayProcessor IO SamplesIQCF32 SamplesIQCF32 a
automaticGainControl tres = toProcessor (agcCreate tres) agcDestroy agcEx

{#pointer firpfbch_crcf as FirPfbch#}

foreign import ccall unsafe "firpfbch_crcf_create_kaiser" c_firpfbch_crcf_create_kaiser
  :: CInt -> CUInt -> CUInt -> CFloat -> IO FirPfbch

foreign import ccall unsafe "firpfbch_crcf_print" c_firpfbch_crcf_print
  :: FirPfbch -> IO ()

foreign import ccall unsafe "firpfbch_crcf_analyzer_execute" c_firpfbch_crcf_analyzer_execute
  :: FirPfbch -> Ptr SamplesIQCF32 -> Ptr SamplesIQCF32 -> IO ()

foreign import ccall unsafe "firpfbch_crcf_destroy" c_firpfbch_crcf_destroy
  :: FirPfbch -> IO ()

{#pointer nco_crcf as Nco#}

foreign import ccall unsafe "nco_crcf_create" c_nco_crcf_create
  :: CInt -> IO Nco

foreign import ccall unsafe "nco_crcf_print" c_nco_crcf_print
  :: Nco -> IO ()

foreign import ccall unsafe "nco_crcf_set_frequency" c_nco_crcf_set_frequency
  :: Nco -> CFloat -> IO ()

foreign import ccall unsafe "nco_crcf_mix_block_down" c_nco_crcf_mix_block_down
  :: Nco -> Ptr SamplesIQCF32 -> Ptr SamplesIQCF32 -> CUInt -> IO ()

foreign import ccall unsafe "nco_crcf_destroy" c_nco_crcf_destroy
  :: Nco -> IO ()

firpfbchCreate :: Int -> IO (FirPfbch, Nco, Int)
firpfbchCreate n = do
  fb <- c_firpfbch_crcf_create_kaiser 0 (fromIntegral n) 7 80.0
  putStrLn "Using polyphase filterbank channelizer:"
  c_firpfbch_crcf_print fb
  nco <- c_nco_crcf_create 1 -- 1 = VCO
  let offset = -0.5 * (fromIntegral n - 1) / fromIntegral n * 2 * pi
  c_nco_crcf_set_frequency nco offset
  putStrLn $ "Offsetting frequency by " ++ show offset ++ " using VCO:"
  c_nco_crcf_print nco
  return (fb, nco, n)

firpfbchDestroy :: (FirPfbch, Nco, Int) -> IO ()
firpfbchDestroy (fb, nco, _) = do
  c_firpfbch_crcf_destroy fb
  c_nco_crcf_destroy nco

firpfbchChan ::
     Storable a
  => (FirPfbch, Nco, Int)
  -> A.Array a
  -> IO [A.Array SamplesIQCF32]
firpfbchChan (fb, nco, nch) a = do
  let nx = A.length a
      nf = nx `div` nch
      x = castPtr . unsafeForeignPtrToPtr $ AT.aStart a
      process s d i =
        allocaArray nch $ \tmp -> do
          let src = s `advancePtr` (nch * i)
              move j = do
                v <- peek (tmp `advancePtr` j)
                poke (d `advancePtr` ((nf * j) + i)) v
          c_firpfbch_crcf_analyzer_execute fb src tmp
          sequence_ [move j | j <- [0 .. nch - 1]]
  fy <- mallocPlainForeignPtrBytes (8 * nx)
  allocaArray nx $ \dx -> do
    c_nco_crcf_mix_block_down nco x dx (fromIntegral nx)
    withForeignPtr fy $ \y -> do
      sequence_ [process dx y i | i <- [0 .. nf - 1]]
      let v =
            AT.Array
              { AT.aStart = fy
              , AT.aEnd = y `plusPtr` (8 * nx)
              , AT.aBound = y `plusPtr` (8 * nx)
              }
          go as = do
            as' <- as
            if A.length as' == nf
              then return (as', Nothing)
              else let (as'', bs) = AT.splitAt nf as'
                    in return (as'', Just bs)
      return $ unfoldr go (Just v)

firpfbchChannelizer ::
     Int -> Processor IO (A.Array SamplesIQCF32) [A.Array SamplesIQCF32] a
firpfbchChannelizer n =
  splitterBy (n * 8 * 1024) .
  toProcessor (firpfbchCreate n) firpfbchDestroy firpfbchChan

distribute_ :: MonadIO m => [FL.Fold m a b] -> FL.Fold m [a] ()
distribute_ fs = FL.Fold step initial extract
  where
    initial =
      mapM (\(FL.Fold s i e) -> i >>= \r -> return (FL.Fold s (return r) e)) fs
    step ss as = do
      zipWithM_ (\(FL.Fold s i _) a -> i >>= \r -> void (s r a)) ss as
      return ss
    extract = mapM_ (\(FL.Fold _ i e) -> i >>= \r -> e r)

mix :: (MonadIO m, Num a, Storable a) => Processor m [A.Array a] (A.Array a) b
mix =
  let f a1 a2 = A.fromList $ zipWith (+) (A.toList a1) (A.toList a2)
   in FL.lmap (foldl1 f)
