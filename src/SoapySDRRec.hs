module Main where

import qualified Data.Map                             as M
import           Data.Maybe                           (mapMaybe)
import           Data.Semigroup                       ((<>))

import           Options.Applicative

import qualified ComposableSDR                        as CS

import qualified Streamly.Prelude                     as S

data Demod
  = DeNo
  | DeNBFM Float
           CS.AudioFormat
  | DeWBFM Int
           CS.AudioFormat
  | DeAM CS.AudioFormat
  deriving (Show, Read)

data Opts = Opts
  { _frequency  :: Double
  , _samplerate :: Double
  , _gain       :: Double
  , _bandwidth  :: Double
  , _numsamples :: Int
  , _outname    :: String
  , _devname    :: String
  , _demod      :: Demod
  , _squelch    :: Float
  }

parser :: Parser Opts
parser =
  Opts <$>
  option
    auto
    (long "frequency" <> short 'f' <> help "Rx frequency in Hz" <> showDefault <>
     value 100.0e6 <>
     metavar "DOUBLE") <*>
  option
    auto
    (long "samplerate" <> short 's' <> help "Sample rate in Hz" <> showDefault <>
     value 2.56e6 <>
     metavar "DOUBLE") <*>
  option
    auto
    (long "gain" <> short 'g' <> help "SDR gain level (0 = auto)" <> showDefault <>
     value 0 <>
     metavar "DOUBLE") <*>
  option
    auto
    (long "bandwidth" <> short 'b' <>
     help
       "Desired output bandwidth in [Hz] (0 = samplerate = no resampling/decimation)" <>
     showDefault <>
     value 0.0 <>
     metavar "DOUBLE") <*>
  option
    auto
    (long "numsamples" <> short 'n' <> help "Number of samples to capture" <>
     showDefault <>
     value 1024 <>
     metavar "INT") <*>
  strOption
    (long "output" <> short 'o' <> showDefault <> value "output" <>
     metavar "FILENAME" <>
     help "Output file(s) name (without extension)") <*>
  strOption
    (long "devname" <> short 'd' <> showDefault <> value "rtlsdr" <>
     metavar "NAME" <>
     help "Soapy device/driver name") <*>
  option
    auto
    (long "demod" <>
     help "Demodulation type" <>
     showDefault <>
     value DeNo) <*>
  option
    auto
    (long "squelch" <> short 's' <>
     help
       "Squelch threshold in [dB]" <>
     showDefault <>
     value 0.0 <>
     metavar "DOUBLE")

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parser <**> helper)
        (fullDesc <>
         progDesc "Dump samples from an SDR via SoapySDR into IQ file" <>
         header "dumpcf32")

sdrProcess :: Opts -> IO ()
sdrProcess opts = do
  src <-
    CS.openSource
      (_devname opts)
      (_samplerate opts)
      (_frequency opts)
      (_gain opts)
  let ns = fromIntegral $ _numsamples opts
      getResampler sr bw
        | bw == 0 = return (id, pure ())
        | otherwise = do
          let resamp_rate = realToFrac (bw / sr)
          rs <- CS.resamplerCreate resamp_rate 60.0
          return (S.mapM (CS.resample rs), CS.resamplerDestroy rs)
      agc =
        if _squelch opts /= 0.0
          then CS.automaticGainControl (_squelch opts)
          else id
  (resampler, resClean) <- getResampler (_samplerate opts) (_bandwidth opts)
  let prep = CS.takeNArr ns . resampler . CS.readChunks
      getAudioSink decim fmt =
        let srOut =
              round
                (if _bandwidth opts == 0
                   then _samplerate opts
                   else _bandwidth opts) `div`
              decim
         in CS.audioFileSink fmt (_outname opts) srOut (_numsamples opts)
      runFold fdl = S.fold fdl (prep src)
  case _demod opts of
    DeNo ->
      let fname = _outname opts ++ ".cf32"
       in runFold (agc $ CS.fileSink fname)
    DeNBFM kf fmt -> runFold (agc . CS.fmDemodulator kf $ getAudioSink 1 fmt)
    DeWBFM decim fmt ->
      runFold
        (agc . CS.wbFMDemodulator (_bandwidth opts) decim $
         getAudioSink decim fmt)
    DeAM fmt -> runFold (agc . CS.amDemodulator $ getAudioSink 1 fmt)
  resClean
  CS.closeSource src

run :: Opts -> IO ()
run opts = do
  devs <- CS.enumerate
  let dnames = mapMaybe (M.lookup "driver" . M.fromList) devs
      dev = _devname opts
  case length dnames of
    0 -> putStrLn "No SDR devices detected"
    _ -> do
      putStrLn $ "Available devices: " ++ show dnames
      if dev `elem` dnames
        then do
          putStrLn $ "Using device: " ++ dev
          sdrProcess opts
        else putStrLn $ "Device " ++ dev ++ " not found"
