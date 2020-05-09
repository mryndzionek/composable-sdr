{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module ComposableSDR
  ( enumerate
  , fileSink
  , stdOutSink
  , audioFileSink
  , openSource
  , readChunks
  , readBytes
  , readFromFile
  , closeSource
  , elemSize
  , resamplerCreate
  , resample
  , resamplerDestroy
  , fmDemodulator
  , wbFMDemodulator
  , stereoFMDecoder
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
  , mux
  , tee
  , addPipe
  , compact
  , delay
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
import           Prelude                                    hiding ((.))

import           Control.Category                           (Category (..))
import           Control.Exception                          (Exception, throwIO)
import           Control.Monad
import qualified Control.Monad.Catch                        as MC
import           Control.Monad.State
import           Data.Complex
import           Data.List                                  (foldl', unfoldr)
import           Data.Typeable

import           Foreign.ForeignPtr (castForeignPtr, plusForeignPtr,
                                     withForeignPtr)
import           GHC.ForeignPtr     (mallocPlainForeignPtrBytes)


import           Foreign.ForeignPtr.Unsafe                  (unsafeForeignPtrToPtr)
import           Foreign.Storable.Tuple()

import           System.IO                                  (stdout)

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
import qualified Streamly.Prelude                           as S

data SoapyException =
  SoapyException
  deriving (Show, Typeable)

data Pipe m a b = forall r. Pipe 
  { _start :: m r
  , _process :: r -> a -> m b
  , _done :: r -> m ()
  }

compose :: Monad m => Pipe m b c -> Pipe m a b -> Pipe m a c
compose (Pipe start1 process1 done1) (Pipe start2 process2 done2) =
  Pipe start process done
  where
    start = (,) <$> start1 <*> start2
    done (r1, r2) = done2 r2 >> done1 r1
    process (r1, r2) = process2 r2 >=> process1 r1

instance Monad m => Category (Pipe m) where
  id = Pipe (return ()) (const return) (const $ return ())
  (.) = compose

instance Monad m => Functor (Pipe m a) where
  fmap f (Pipe start process done) =
    Pipe start (\r a -> fmap f (process r a)) done

instance Exception SoapyException

newtype Array a = Array
  { fromArray :: AT.Array a
  }

toArray :: AT.Array a -> Array a
toArray = Array

mapA :: (Storable a, Storable b) => (a -> b) -> A.Array a -> A.Array b
mapA f = A.fromList . fmap f . A.toList

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

type ArrayPipe m a b = Pipe m (A.Array a) (A.Array b)
type Demodulator = ArrayPipe IO SamplesIQCF32 Float

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

compact ::
     Storable a => Int -> FL.Fold IO (A.Array a) b -> FL.Fold IO (A.Array a) b
compact n (FL.Fold step1 start1 done1) = FL.Fold step start done
  where
    start = do
      s <- start1
      let a0 = A.fromList []
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
                    let a0 = A.fromList []
                    return (ba, a0)
                  else return $ AT.splitAt n ba
              s' <- step1 s a'
              return (s', b')
            else do
              ba <- AT.spliceTwo b a
              return (s, ba)

delay ::
     (MonadIO m, Storable a, Num a)
  => Int
  -> FL.Fold m (A.Array (a, a)) b
  -> FL.Fold m (A.Array a) b
delay n (FL.Fold step1 start1 done1) = FL.Fold step start done
  where
    start = do
      s <- start1
      let a0 = A.fromList (replicate n 0)
      return (s, a0)
    done (s, b) = do
      s' <- step1 s (A.fromList $ zip (A.toList b) (repeat 0))
      done1 s'
    step (s, b) a = do
      ba <- AT.spliceTwo b a
      let (a', b') = AT.splitAt (A.length a - n) ba
      s' <- step1 s (A.fromList $ zip (A.toList a) (A.toList a'))
      return (s', b')

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

-- TODO see if changing this into a Pipe gets us somewhere
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

addPipe :: MonadIO m => Pipe m a b -> FL.Fold m b c -> FL.Fold m a c
addPipe (Pipe creat process dest) (FL.Fold step1 start1 done1) =
  FL.Fold step start done
  where
    start = do
      s <- start1
      r <- creat
      return (s, r, process r)
    done (s, r, _) = do
      dest r
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

fmDemodulator :: Float -> Demodulator 
fmDemodulator kf = Pipe (fmdemodCreate kf) fmDemod fmdemodDestroy

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

amDemodulator :: Demodulator
amDemodulator = Pipe amdemodCreate amDemod amdemodDestroy

openAudioFile :: AudioFormat -> FilePath -> Int -> Int -> Int -> IO SF.Handle
openAudioFile fmt fp sr sn nch = SF.openFile (fp ++ ext) SF.WriteMode info
  where
    sfFmt AU  = (SF.HeaderFormatAu, ".au")
    sfFmt WAV = (SF.HeaderFormatWav, ".wav")
    (sff, ext) = sfFmt fmt
    info =
      SF.Info sn sr nch (SF.Format sff SF.SampleFormatFloat SF.EndianBig) 1 False

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
  -> Int
  -> String
  -> FL.Fold m (A.Array a) ()
audioFileSink fmt sr sn nch fp =
  let wav = openAudioFile fmt fp sr sn nch
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
  f <- c_firdecim_rrrf_create_kaiser (fromIntegral m) 10 60.0
  putStrLn "Using output decimator:"
  c_firdecim_rrrf_print f
  return f

firdecimDestroy :: FirDecim -> IO ()
firdecimDestroy = c_firdecim_rrrf_destroy

firDecim :: MonadIO m => Int -> FirDecim -> A.Array Float -> m (A.Array Float)
firDecim m f a =
  let n = fromIntegral (A.length a) `div` m
   in wrap n (4 * n) (c_firdecim_rrrf_execute_block f) a

firDecimator :: Int -> ArrayPipe IO Float Float
firDecimator m = Pipe (firdecimCreate m) (firDecim m) firdecimDestroy

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

realToComplex :: ArrayPipe IO Float SamplesIQCF32
realToComplex = Pipe firhilbCreate firhilbDecim firhilbDestroy 

firhilbInterp ::
     MonadIO m => FirHilb -> A.Array SamplesIQCF32 -> m (A.Array Float)
firhilbInterp f a =
  let n = (fromIntegral $ A.length a)
   in wrap n (2 * 4 * n) (c_firhilbf_interp_execute_block f) a

complexToReal :: ArrayPipe IO SamplesIQCF32 Float
complexToReal = Pipe firhilbCreate firhilbInterp firhilbDestroy

foreign import ccall unsafe "liquid_iirdes" c_liquid_iirdes
  :: CInt ->
  CInt ->
    CInt ->
      CUInt ->
        Float -> Float -> Float -> Float -> Ptr Float -> Ptr Float -> IO ()

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

foreign import ccall unsafe "iirfilt_rrrf_create_sos" c_iirfilt_rrrf_create_sos
  :: Ptr Float -> Ptr Float -> CUInt -> IO IirFilt

foreign import ccall unsafe "iirfilt_rrrf_print" c_iirfilt_rrrf_print
  :: IirFilt -> IO ()

foreign import ccall unsafe "iirfilt_rrrf_execute_block" c_iirfilt_rrrf_execute_block
  :: IirFilt -> Ptr Float -> CUInt -> Ptr Float -> IO ()

foreign import ccall unsafe "iirfilt_rrrf_destroy" c_iirfilt_rrrf_destroy
  :: IirFilt -> IO ()

iirDes :: Int -> Float -> Float -> Float -> Float -> IO IirFilt
iirDes n fc f0 ap' as = do
  let r = n `mod` 2 -- odd/even order
      l = (n - r) `div` 2 -- filter semi-length
      hLen = 3 * (l + r)
  allocaArray hLen $ \b ->
    allocaArray hLen $ \a
            -- LIQUID_IIRDES_BUTTER LIQUID_IIRDES_LOWPASS LIQUID_IIRDES_SOS
     -> do
      c_liquid_iirdes 0 0 0 (fromIntegral n) fc f0 ap' as b a
      c_iirfilt_rrrf_create_sos b a (fromIntegral $ l + r)

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

iirFilter :: [Float] -> [Float] -> ArrayPipe IO Float Float
iirFilter bt at = Pipe (iirfiltCreate bt at) iirFilt iirfiltDestroy

iirDeemphFilter ::
     Int
  -> Float
  -> Float
  -> Float
  -> Float
  -> Pipe IO (A.Array Float) (A.Array Float)
iirDeemphFilter n fc f0 ap' as =
  Pipe (iirDes n fc f0 ap' as) iirFilt iirfiltDestroy

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

dcBlocker :: ArrayPipe IO SamplesIQCF32 SamplesIQCF32
dcBlocker = Pipe dcBlockerCreate iirCFilt iirfiltCDestroy

iirfiltCDestroy :: IirFiltC -> IO ()
iirfiltCDestroy = c_iirfilt_crcf_destroy

wbFMDemodulator :: Double -> Int -> Demodulator
wbFMDemodulator quadRate decim =
  let iirDeemph = iirDeemphFilter 2 (realToFrac $ 5000 / quadRate) 0.0 10.0 10.0
   in firDecimator decim . iirDeemph . fmDemodulator 0.6

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
        -- /= LIQUID_AGC_SQUELCH_SIGNALHI
        when (s /= 3) (poke y (0 :+ 0)) -- squelch enabled
  zipWithM_ go xsp ysp

agcCreate :: Float -> IO Agc
agcCreate tres = do
  a <- c_agc_crcf_create
  c_agc_crcf_set_bandwidth a 0.1
  c_agc_crcf_set_signal_level a 1e-3
  c_agc_crcf_squelch_enable a
  c_agc_crcf_squelch_set_threshold a tres
  c_agc_crcf_squelch_set_timeout a 1000
  putStrLn "Using AGC:"
  c_agc_crcf_print a
  return a

agcDestroy :: Agc -> IO ()
agcDestroy = c_agc_crcf_destroy

agcEx :: MonadIO m => Agc -> A.Array SamplesIQCF32 -> m (A.Array SamplesIQCF32)
agcEx agc a =
  let n = (fromIntegral $ A.length a)
   in wrap n (8 * n) (agcExecuteBlock agc) a

automaticGainControl :: Float -> ArrayPipe IO SamplesIQCF32 SamplesIQCF32
automaticGainControl tres = Pipe (agcCreate tres) agcEx agcDestroy

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
  :: Nco -> Float -> IO ()

foreign import ccall unsafe "nco_crcf_pll_set_bandwidth" c_nco_crcf_pll_set_bandwidth
  :: Nco -> Float -> IO ()

foreign import ccall unsafe "nco_crcf_pll_step" c_nco_crcf_pll_step
  :: Nco -> Float -> IO ()

foreign import ccall unsafe "nco_crcf_step" c_nco_crcf_step
  :: Nco -> IO ()

foreign import ccall unsafe "nco_crcf_cexpf" c_nco_crcf_cexpf
  :: Nco -> Ptr SamplesIQCF32 -> IO ()

foreign import ccall unsafe "nco_crcf_get_phase" c_nco_crcf_get_phase
  :: Nco -> IO Float

foreign import ccall unsafe "nco_crcf_set_phase" c_nco_crcf_set_phase
  :: Nco -> Float -> IO ()

foreign import ccall unsafe "nco_crcf_mix_block_down" c_nco_crcf_mix_block_down
  :: Nco -> Ptr SamplesIQCF32 -> Ptr SamplesIQCF32 -> CUInt -> IO ()

foreign import ccall unsafe "nco_crcf_mix_block_up" c_nco_crcf_mix_block_up
  :: Nco -> Ptr SamplesIQCF32 -> Ptr SamplesIQCF32 -> CUInt -> IO ()

foreign import ccall unsafe "nco_crcf_destroy" c_nco_crcf_destroy
  :: Nco -> IO ()

ncoCreate :: Float -> IO Nco
ncoCreate f = do
  nco <- c_nco_crcf_create 1 -- 1 == VCO
  c_nco_crcf_set_frequency nco f
  putStrLn "Using NCO (VCO):"
  c_nco_crcf_print nco
  return nco

ncoDestroy :: Nco -> IO ()
ncoDestroy = c_nco_crcf_destroy

ncoMixDown ::
     MonadIO m => Nco -> A.Array SamplesIQCF32 -> m (A.Array SamplesIQCF32)
ncoMixDown nco a =
  let n = (fromIntegral $ A.length a)
   in wrap n (8 * n) (\x m y -> c_nco_crcf_mix_block_down nco x y m) a

mixDown :: Float -> ArrayPipe IO SamplesIQCF32 SamplesIQCF32
mixDown f = Pipe (ncoCreate f) ncoMixDown ncoDestroy

ncoMixUp ::
     MonadIO m => Nco -> A.Array SamplesIQCF32 -> m (A.Array SamplesIQCF32)
ncoMixUp nco a =
  let n = (fromIntegral $ A.length a)
   in wrap n (8 * n) (\x m y -> c_nco_crcf_mix_block_up nco x y m) a

mixUp :: Float -> ArrayPipe IO SamplesIQCF32 SamplesIQCF32
mixUp f = Pipe (ncoCreate f) ncoMixUp ncoDestroy

firpfbchCreate :: Int -> IO (FirPfbch, Nco, Int)
firpfbchCreate n = do
  fb <- c_firpfbch_crcf_create_kaiser 0 (fromIntegral n) 7 80.0
  putStrLn "Using polyphase filterbank channelizer:"
  c_firpfbch_crcf_print fb
  nco <- c_nco_crcf_create 1
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
     Int -> Pipe IO (A.Array SamplesIQCF32) [A.Array SamplesIQCF32]
firpfbchChannelizer n = Pipe (firpfbchCreate n) firpfbchChan firpfbchDestroy

distribute_ :: MonadIO m => [FL.Fold m a b] -> FL.Fold m [a] ()
distribute_ fs = FL.Fold step initial extract
  where
    initial =
      mapM (\(FL.Fold s i e) -> i >>= \r -> return (FL.Fold s (return r) e)) fs
    step ss as = do
      zipWithM_ (\(FL.Fold s i _) a -> i >>= \r -> void (s r a)) ss as
      return ss
    extract = mapM_ (\(FL.Fold _ i e) -> i >>= \r -> e r)

mix :: (MonadIO m, Num a, Storable a) => Pipe m [A.Array a] (A.Array a)
mix =
  let f a1 a2 = A.fromList $ zipWith (+) (A.toList a1) (A.toList a2)
   in foldl1 f <$> Control.Category.id

mux :: Monad m => [Pipe m a b] -> Pipe m [a] [b]
mux ps = Pipe start process done
  where
    start = mapM (\(Pipe s p d) -> s >>= \r -> return (Pipe (return r) p d)) ps
    process = zipWithM (\(Pipe s p _) a -> s >>= \r -> p r a)
    done = mapM_ (\(Pipe s _ d) -> s >>= \r -> d r)

tee ::
     (Monad m, Storable b, Storable c)
  => ArrayPipe m a b
  -> ArrayPipe m a c
  -> ArrayPipe m a (b, c)
tee (Pipe start1 process1 done1) (Pipe start2 process2 done2) =
  Pipe start process done
  where
    start = (,) <$> start1 <*> start2
    done (r1, r2) = done2 r2 >> done1 r1
    process (r1, r2) a = do
      b <- process1 r1 a
      c <- process2 r2 a
      return $ A.fromList $ zip (A.toList b) (A.toList c)

lMapA :: (Storable a, Storable t) => (t -> a) -> ArrayPipe m a b -> ArrayPipe m t b
lMapA f (Pipe start process done) = Pipe start proc done
  where
    proc r a = process r (mapA f a)

{#pointer firfilt_crcf as FirFiltC#}

foreign import ccall unsafe "firfilt_crcf_create_kaiser" c_firfilt_crcf_create_kaiser
  :: CUInt -> Float -> Float -> Float -> IO FirFiltC

foreign import ccall unsafe "firfilt_crcf_print" c_firfilt_crcf_print
  :: FirFiltC -> IO ()

foreign import ccall unsafe "firfilt_crcf_set_scale" c_firfilt_crcf_set_scale
  :: FirFiltC -> Float -> IO ()

foreign import ccall unsafe "firfilt_crcf_groupdelay" c_firfilt_crcf_groupdelay
  :: FirFiltC -> Float -> IO Float

foreign import ccall unsafe "firfilt_crcf_execute_block" c_firfilt_crcf_execute_block
  :: FirFiltC ->
  Ptr SamplesIQCF32 -> CUInt -> Ptr SamplesIQCF32 -> IO ()

foreign import ccall unsafe "firfilt_crcf_destroy" c_firfilt_crcf_destroy
  :: FirFiltC -> IO ()

firfiltCreateCKaiser :: CUInt -> Float -> Float -> Float -> IO FirFiltC
firfiltCreateCKaiser n fc as mu = do
  f <- c_firfilt_crcf_create_kaiser n fc as mu
  c_firfilt_crcf_set_scale f (2.0 * fc)
  putStrLn "Using FIR filter:"
  c_firfilt_crcf_print f
  return f

firfiltCDestroy :: FirFiltC -> IO ()
firfiltCDestroy = c_firfilt_crcf_destroy

firFiltC :: MonadIO m => FirFiltC -> A.Array SamplesIQCF32 -> m (A.Array SamplesIQCF32)
firFiltC f a =
  let n = (fromIntegral $ A.length a)
   in wrap n (8 * n) (c_firfilt_crcf_execute_block f) a

firFilterCKaiser ::
     CUInt
  -> Float
  -> Float
  -> Float
  -> Pipe IO (A.Array SamplesIQCF32) (A.Array SamplesIQCF32)
firFilterCKaiser n fc as mu =
  Pipe (firfiltCreateCKaiser n fc as mu) firFiltC firfiltCDestroy

firFilterC ::
     FirFiltC -> Pipe IO (A.Array SamplesIQCF32) (A.Array SamplesIQCF32)
firFilterC f = Pipe (return f) firFiltC firfiltCDestroy

pllCreate :: Float -> Float -> IO (Nco, Nco)
pllCreate f bw = do
  ncoPilotExact <- c_nco_crcf_create 1
  c_nco_crcf_set_frequency ncoPilotExact f
  c_nco_crcf_print ncoPilotExact
  c_nco_crcf_pll_set_bandwidth ncoPilotExact bw
  ncoStereoSubcarrier <- c_nco_crcf_create 1
  c_nco_crcf_set_frequency ncoStereoSubcarrier (2 * f)
  putStrLn "Using FMS PLL:"
  c_nco_crcf_print ncoPilotExact
  c_nco_crcf_print ncoStereoSubcarrier
  return (ncoPilotExact, ncoStereoSubcarrier)

pllDestroy :: (Nco, Nco) -> IO ()
pllDestroy (ncoPE, ncoSS) = do
  c_nco_crcf_destroy ncoSS
  c_nco_crcf_destroy ncoPE

pllStep :: (Nco, Nco) -> (SamplesIQCF32, SamplesIQCF32) -> IO SamplesIQCF32
pllStep (ncoPE, ncoSS) (p, s) = do
  phi <- c_nco_crcf_get_phase ncoPE
  c_nco_crcf_set_phase ncoSS (2 * phi)
  alloca $ \c -> do
    c_nco_crcf_cexpf ncoPE c
    c' <- peek c
    let (CFloat perr) = phase $ p * conjugate c'
    c_nco_crcf_pll_step ncoPE perr
    c_nco_crcf_step ncoPE
    alloca $ \a ->
      alloca $ \b -> do
        poke a s
        c_nco_crcf_mix_block_down ncoSS a b 1
        peek b

pllEx ::
     MonadIO m
  => (Nco, Nco)
  -> A.Array (SamplesIQCF32, SamplesIQCF32)
  -> m (A.Array SamplesIQCF32)
pllEx ns a = liftIO $ A.fromList <$> mapM (pllStep ns) (A.toList a)

fmsPll ::
     Float -> Float -> ArrayPipe IO (SamplesIQCF32, SamplesIQCF32) SamplesIQCF32
fmsPll f bw = Pipe (pllCreate f bw) pllEx pllDestroy

-- loosely based on https://github.com/windytan/wfm-tools
stereoFMDecoder' ::
     Double
  -> Int
  -> IO (Pipe IO (A.Array (Float, Float)) (A.Array (Float, Float)), Int)
stereoFMDecoder' quadRate decim = do
  let kPilotHz = 19000.0
      kPLLBandwidthHz = 9.0
      kPilotFIRHalfbandHz = 800.0
      kAudioFIRCutoffHz = 15000.0
      kDeEmphasisOrder = 2
      kDeEmphasisCutoffHz = 5000.0
      kStereoGain = 2.0
      ncoF = realToFrac $ kPilotHz * 2 * pi / quadRate
      firFilt n fc = firFilterCKaiser (round n) (realToFrac fc) 60.0 0.0
      approxMixDown = mixDown ncoF
      approxMixUp = mixUp ncoF
      pll = fmsPll ncoF (realToFrac $ kPLLBandwidthHz / quadRate)
      ex (CFloat a) = a
      firLPlusR =
        mapA (ex . realPart) <$>
        firFilt (quadRate / 1350.0) (kAudioFIRCutoffHz / quadRate)
      firLMinusR =
        mapA ((* kStereoGain) . ex . realPart) <$>
        firFilt (quadRate / 1350.0) (kAudioFIRCutoffHz / quadRate)
      iirDeemphL =
        iirDeemphFilter
          kDeEmphasisOrder
          (realToFrac $ kDeEmphasisCutoffHz / quadRate)
          0.0
          10.0
          10.0
      iirDeemphR =
        iirDeemphFilter
          kDeEmphasisOrder
          (realToFrac $ kDeEmphasisCutoffHz / quadRate)
          0.0
          10.0
          10.0
  fp <-
    firfiltCreateCKaiser
      (round $ quadRate / 1350.0)
      (realToFrac $ kPilotFIRHalfbandHz / quadRate)
      60.0
      0.0
  gDelay <- c_firfilt_crcf_groupdelay fp (realToFrac $ 100 / quadRate)
  let firPilot = firFilterC fp
      wire = Control.Category.id
      prep = approxMixUp . firPilot . approxMixDown
      process =
        tee
          (lMapA fst (firDecimator decim . iirDeemphL))
          (lMapA snd (firDecimator decim . iirDeemphR)) .
        (mapA (\(x, y) -> (y + x, y - x)) <$>
         tee
           (firLMinusR .
            pll .
            tee
              (lMapA ((:+ 0) . CFloat . fst) prep)
              (lMapA ((:+ 0) . CFloat . snd) wire))
           (lMapA ((:+ 0) . CFloat . snd) firLPlusR))
  return (process, round gDelay)

stereoFMDecoder ::
     Double
  -> Int
  -> FL.Fold IO (A.Array Float) b
  -> IO (FL.Fold IO (A.Array Float) b)
stereoFMDecoder quadRate decim sink = do
  (dem, d) <- stereoFMDecoder' quadRate decim
  let snk =
        FL.lmapM
          (return . A.fromList . concatMap (\(x, y) -> [x, y]) . A.toList)
          sink
  return $ delay d (addPipe dem snk)
