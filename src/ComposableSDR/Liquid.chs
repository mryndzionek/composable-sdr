module ComposableSDR.Liquid
  ( resampler
  , symSyncR
  , symSyncC
  , symTracker
  , mixUp
  , mixDown
  , fmDemodulator
  , wbFMDemodulator
  , stereoFMDecoder
  , amDemodulator
  , fskDemodulator
  , gmskDemodulator
  , gmskDemWithSync
  , firDecimator
  , automaticGainControl
  , firFilterR
  , firFilterRNyquist
  , iirFilter
  , dcBlocker
  , firpfbchChannelizer
  , realToComplex
  , complexToReal
  ) where

import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array                hiding (newArray)
import           Prelude                              hiding ((.))

import           Control.Category                     (Category (..))
import           Control.Monad
import           Control.Exception                    (throwIO)

import           Data.Complex
import           Data.List                            (unfoldr)

import           GHC.ForeignPtr                       (mallocPlainForeignPtrBytes)

import           Foreign.ForeignPtr.Unsafe            (unsafeForeignPtrToPtr)

import qualified Streamly.Internal.Data.Fold.Types    as FL
import qualified Streamly.Internal.Memory.Array.Types as AT (Array (..),
                                                             shrinkToFit,
                                                             splitAt)
import qualified Streamly.Memory.Array                as A

import           ComposableSDR.Common
import           ComposableSDR.Trans
import           ComposableSDR.Types

#include <SoapySDR/Device.h>
#include <SoapySDR/Formats.h>
#include <liquid/liquid.h>

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

resampler ::
     Float -> Float -> Pipe IO (A.Array SamplesIQCF32) (A.Array SamplesIQCF32)
resampler r as = Pipe (resamplerCreate r as) resample resamplerDestroy

{#pointer symtrack_cccf as SymTrackCccf#}

foreign import ccall unsafe "symtrack_cccf_create" c_symtrack_cccf_create
  :: Int -> CUInt -> CUInt -> Float -> Int -> IO SymTrackCccf

foreign import ccall unsafe "symtrack_cccf_print" c_symtrack_cccf_print
  :: SymTrackCccf -> IO ()

foreign import ccall unsafe "symtrack_cccf_execute_block" c_symtrack_cccf_execute_block
  :: SymTrackCccf ->
  Ptr SamplesIQCF32 ->
    CUInt -> Ptr SamplesIQCF32 -> Ptr CUInt -> IO ()

foreign import ccall unsafe "symtrack_cccf_destroy" c_symtrack_cccf_destroy
  :: SymTrackCccf -> IO ()

trackSym :: (MonadIO m) => SymTrackCccf -> AT.Array SamplesIQCF32 -> m (AT.Array SamplesIQCF32)
trackSym st x =
  liftIO $ do
    let nx = fromIntegral $ A.length x
        ny = fromIntegral nx
    fy <- mallocPlainForeignPtrBytes (elemSize * ny)
    withForeignPtr fy $ \y ->
      alloca $ \pnwy -> do
        c_symtrack_cccf_execute_block
          st
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

symTrackCreate :: CUInt -> CUInt -> IO SymTrackCccf
symTrackCreate m k = do
  let ftype = 9 -- LIQUID_FIRFILT_RRC
      beta = 0.25
      ms = 21 -- LIQUID_MODEM_BPSK
  st <- c_symtrack_cccf_create ftype k m beta ms
  putStrLn "Using symbol tracker:"
  c_symtrack_cccf_print st
  return st

symTrackDestroy :: SymTrackCccf -> IO ()
symTrackDestroy st
  | st == nullPtr = return ()
  | otherwise = c_symtrack_cccf_destroy st

symTracker ::
     CUInt -> CUInt -> Pipe IO (A.Array SamplesIQCF32) (A.Array SamplesIQCF32)
symTracker m k = Pipe (symTrackCreate m k) trackSym symTrackDestroy

{#pointer symsync_crcf as SymSyncCrcf#}

foreign import ccall unsafe "symsync_crcf_create_rnyquist" c_symsync_crcf_create_rnyquist
  :: Int -> CUInt -> CUInt -> Float -> CUInt -> IO SymSyncCrcf

foreign import ccall unsafe "symsync_crcf_print" c_symsync_crcf_print
  :: SymSyncCrcf -> IO ()

foreign import ccall unsafe "symsync_crcf_set_lf_bw" c_symsync_crcf_set_lf_bw
  :: SymSyncCrcf -> Float -> IO ()

foreign import ccall unsafe "symsync_crcf_execute" c_symsync_crcf_execute
  :: SymSyncCrcf ->
  Ptr SamplesIQCF32 ->
    CUInt -> Ptr SamplesIQCF32 -> Ptr CUInt -> IO ()

foreign import ccall unsafe "symsync_crcf_destroy" c_symsync_crcf_destroy
  :: SymSyncCrcf -> IO ()

syncSym ::
     ( MonadIO m
     , Storable a
     , Storable b
     )
  => (t2 -> Ptr a -> CUInt -> Ptr b -> Ptr CUInt -> IO ())
  -> Int
  -> t2
  -> A.Array a
  -> m (A.Array b)
syncSym ex s st x =
  liftIO $ do
    let nx = fromIntegral $ A.length x
        ny = fromIntegral nx
    fy <- mallocPlainForeignPtrBytes (s * ny)
    withForeignPtr fy $ \y ->
      alloca $ \pnwy -> do
        ex st (castPtr . unsafeForeignPtrToPtr $ AT.aStart x) nx y pnwy
        nwy <- fromIntegral <$> peek pnwy
        let v =
              AT.Array
                { AT.aStart = fy
                , AT.aEnd = y `plusPtr` (s * nwy)
                , AT.aBound = y `plusPtr` (s * ny)
                }
        AT.shrinkToFit v

symSyncCCreate :: CUInt -> CUInt -> IO SymSyncCrcf
symSyncCCreate m k = do
  let ftype = 7 -- LIQUID_FIRFILT_ARKAISER
      beta = 0.5
      nf = 32
  ss <- c_symsync_crcf_create_rnyquist ftype k m beta nf
  -- c_symsync_crcf_set_lf_bw ss 0.3
  putStrLn "Using symbol sync:"
  c_symsync_crcf_print ss
  return ss

symSyncCDestroy :: SymSyncCrcf -> IO ()
symSyncCDestroy ss
  | ss == nullPtr = return ()
  | otherwise = c_symsync_crcf_destroy ss

symSyncC ::
     CUInt -> CUInt -> Pipe IO (A.Array SamplesIQCF32) (A.Array SamplesIQCF32)
symSyncC m k =
  Pipe (symSyncCCreate m k) (syncSym c_symsync_crcf_execute elemSize) symSyncCDestroy

{#pointer symsync_rrrf as SymSyncRrrf#}

foreign import ccall unsafe "symsync_rrrf_create_kaiser" c_symsync_rrrf_create_kaiser
  :: CUInt -> CUInt -> Float -> CUInt -> IO SymSyncRrrf

foreign import ccall unsafe "symsync_rrrf_print" c_symsync_rrrf_print
  :: SymSyncRrrf -> IO ()

-- foreign import ccall unsafe "symsync_rrrf_set_lf_bw" c_symsync_rrrf_set_lf_bw
--   :: SymSyncRrrf -> Float -> IO ()

foreign import ccall unsafe "symsync_rrrf_set_output_rate" c_symsync_rrrf_set_output_rate
  :: SymSyncRrrf -> CUInt -> IO ()

foreign import ccall unsafe "symsync_rrrf_execute" c_symsync_rrrf_execute
  :: SymSyncRrrf ->
  Ptr Float -> CUInt -> Ptr Float -> Ptr CUInt -> IO ()

foreign import ccall unsafe "symsync_rrrf_destroy" c_symsync_rrrf_destroy
  :: SymSyncRrrf -> IO ()

symSyncRCreate :: CUInt -> CUInt -> Float -> CUInt -> IO SymSyncRrrf
symSyncRCreate k m beta _m = do
  ss <- c_symsync_rrrf_create_kaiser k m beta _m
  c_symsync_crcf_set_lf_bw ss 0.05
  c_symsync_rrrf_set_output_rate ss 2
  putStrLn "Using symbol sync:"
  c_symsync_rrrf_print ss
  return ss

symSyncRDestroy :: SymSyncRrrf -> IO ()
symSyncRDestroy ss
  | ss == nullPtr = return ()
  | otherwise = c_symsync_rrrf_destroy ss

symSyncR ::
     CUInt -> CUInt -> Float -> CUInt -> Pipe IO (A.Array Float) (A.Array Float)
symSyncR k m beta _m =
  Pipe (symSyncRCreate k m beta _m) (syncSym c_symsync_rrrf_execute 4) symSyncRDestroy

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

fmDemod ::
     (MonadIO m) => FreqDem -> AT.Array SamplesIQCF32 -> m (AT.Array Float)
fmDemod d a =
  let n = fromIntegral $ A.length a
   in wrap n (4 * n) (c_freqdem_demodulate_block d) a

fmdemodDestroy :: FreqDem -> IO ()
fmdemodDestroy = c_freqdem_crcf_destroy

fmDemodulator :: Float -> Demodulator 
fmDemodulator kf = Pipe (fmdemodCreate kf) fmDemod fmdemodDestroy

{#pointer fskdem as FskDem#}

foreign import ccall unsafe "fskdem_create" c_fskdem_create
  :: CUInt -> CUInt -> Float -> IO FskDem

foreign import ccall unsafe "fskdem_print" c_fskdem_print
  :: FskDem -> IO ()

foreign import ccall unsafe "fskdem_demodulate" c_fskdem_demodulate
  :: FskDem -> Ptr SamplesIQCF32 -> IO CUInt

foreign import ccall unsafe "fskdem_destroy" c_fskdem_crcf_destroy
  :: FskDem -> IO ()

fskdem_demodulate_block ::
     (FreqDem, CUInt) -> Ptr SamplesIQCF32 -> CUInt -> Ptr CUInt -> IO ()
fskdem_demodulate_block (d, k) x n y = sequence_ $ fmap procs [0 .. n - 1]
  where
    procs i = do
      let src = x `advancePtr` (fromIntegral k * fromIntegral i)
          dst = y `advancePtr` fromIntegral i
      v <- c_fskdem_demodulate d src
      poke dst v

fskdemodCreate :: CUInt -> CUInt -> Float -> IO (FreqDem, CUInt)
fskdemodCreate m k bw = do
  d <- c_fskdem_create m k bw
  putStrLn "Using FSK demodulator:"
  c_fskdem_print d
  return (d, k)

fskDemod ::
     (MonadIO m)
  => (FreqDem, CUInt)
  -> AT.Array SamplesIQCF32
  -> m (AT.Array CUInt)
fskDemod (d, k) a =
  let n = fromIntegral $ A.length a
   in do -- liftIO $ when (n `rem` k /= 0) (throwIO SoapyException)
         let n' = fromIntegral $ n `div` k
         wrap n' (4 * n') (fskdem_demodulate_block (d, k)) a

fskdemodDestroy :: (FreqDem, CUInt) -> IO ()
fskdemodDestroy (d, _) = c_fskdem_crcf_destroy d

fskDemodulator :: CUInt -> CUInt -> Float -> ArrayPipe IO SamplesIQCF32 CUInt
fskDemodulator m k bw = Pipe (fskdemodCreate m k bw) fskDemod fskdemodDestroy

{#pointer gmskdem as GmskDem#}

foreign import ccall unsafe "gmskdem_create" c_gmskdem_create
  :: CUInt -> CUInt -> Float -> IO GmskDem

foreign import ccall unsafe "gmskdem_print" c_gmskdem_print
  :: GmskDem -> IO ()

foreign import ccall unsafe "gmskdem_demodulate" c_gmskdem_demodulate
  :: GmskDem -> Ptr SamplesIQCF32 -> Ptr CUInt -> IO ()

foreign import ccall unsafe "gmskdem_destroy" c_gmskdem_crcf_destroy
  :: GmskDem -> IO ()

gmskdem_demodulate_block ::
     (GmskDem, CUInt) -> Ptr SamplesIQCF32 -> CUInt -> Ptr CUInt -> IO ()
gmskdem_demodulate_block (d, k) x n y = sequence_ $ fmap procs [0 .. n - 1]
  where
    procs i = do
      let src = x `advancePtr` (fromIntegral k * fromIntegral i)
          dst = y `advancePtr` fromIntegral i
      c_gmskdem_demodulate d src dst

gmskdemodCreate :: CUInt -> CUInt -> Float -> IO (FreqDem, CUInt)
gmskdemodCreate m k bw = do
  d <- c_gmskdem_create k m bw
  putStrLn "Using GMSK demodulator:"
  c_gmskdem_print d
  return (d, k)

gmskDemod ::
     (MonadIO m)
  => (FreqDem, CUInt)
  -> AT.Array SamplesIQCF32
  -> m (AT.Array CUInt)
gmskDemod (d, k) a =
  let n = fromIntegral $ A.length a
   in do liftIO $ when (n `rem` k /= 0) (throwIO SoapyException)
         let n' = fromIntegral $ n `div` k
         wrap n' (4 * n') (gmskdem_demodulate_block (d, k)) a

gmskdemodDestroy :: (FreqDem, CUInt) -> IO ()
gmskdemodDestroy (d, _) = c_gmskdem_crcf_destroy d

gmskDemodulator :: CUInt -> CUInt -> Float -> ArrayPipe IO SamplesIQCF32 CUInt
gmskDemodulator m k bw = Pipe (gmskdemodCreate m k bw) gmskDemod gmskdemodDestroy

gmskDemWithSync :: CUInt -> Pipe IO (A.Array SamplesIQCF32) (A.Array Float)
gmskDemWithSync k =
  let -- mf = firFilterRNyquist k 8 0.3 0
      ss = symSyncR k 4 0 64
      d = fmDemodulator (0.01 * fromIntegral k)
   in ss . d

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
  let n = 2 * (fromIntegral $ A.length a)
   in wrap n (4 * n) (c_firhilbf_interp_execute_block f) a

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

{#pointer firfilt_rrrf as FirFiltR#}

foreign import ccall unsafe "firfilt_rrrf_create_rnyquist" c_firfilt_rrrf_create_rnyquist
  :: Int -> CUInt -> CUInt -> Float -> Float -> IO FirFiltR

foreign import ccall unsafe "firfilt_rrrf_print" c_firfilt_rrrf_print
  :: FirFiltR -> IO ()

-- foreign import ccall unsafe "firfilt_rrrf_set_scale" c_firfilt_rrrf_set_scale
--   :: FirFiltR -> Float -> IO ()

foreign import ccall unsafe "firfilt_rrrf_execute_block" c_firfilt_rrrf_execute_block
  :: FirFiltR -> Ptr Float -> CUInt -> Ptr Float -> IO ()

foreign import ccall unsafe "firfilt_rrrf_destroy" c_firfilt_rrrf_destroy
  :: FirFiltR -> IO ()

firfiltCreateRNyquist :: CUInt -> CUInt -> Float -> Float -> IO FirFiltR
firfiltCreateRNyquist k m beta mu = do
  f <- c_firfilt_rrrf_create_rnyquist 12 k m beta mu -- LIQUID_FIRFILT_GMSKRX
  c_firfilt_crcf_set_scale f (1.0 / fromIntegral k)
  putStrLn "Using FIR filter:"
  c_firfilt_rrrf_print f
  return f

firfiltRDestroy :: FirFiltR -> IO ()
firfiltRDestroy = c_firfilt_rrrf_destroy

firFiltR :: MonadIO m => FirFiltR -> A.Array Float -> m (A.Array Float)
firFiltR f a =
  let n = (fromIntegral $ A.length a)
   in wrap n (4 * n) (c_firfilt_rrrf_execute_block f) a

firFilterRNyquist :: CUInt -> CUInt -> Float -> Float -> Pipe IO (A.Array Float) (A.Array Float)
firFilterRNyquist k m beta mu =
  Pipe (firfiltCreateRNyquist k m beta mu) firFiltR firfiltRDestroy

firFilterR ::
     FirFiltR -> Pipe IO (A.Array Float) (A.Array Float)
firFilterR f = Pipe (return f) firFiltR firfiltRDestroy

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
