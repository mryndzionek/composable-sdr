{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)
import           Data.Semigroup      ((<>))

import           Control.Category    (Category (..), id)
import           Control.Exception   (fromException, try)

import           Options.Applicative
import           Prelude             hiding (id, (.))

import qualified ComposableSDR       as CS

import qualified Streamly.Prelude    as S

data Demod
  = DeNo
  | DeNBFM Float
           CS.AudioFormat
  | DeWBFM Int
           CS.AudioFormat
  | DeFMS Int
          CS.AudioFormat
  | DeAM CS.AudioFormat
  deriving (Show, Read)

data SoapySDRInputCfg = SoapySDRInputCfg
  { _devname   :: String
  , _frequency :: Double
  , _gain      :: Double
  } deriving (Show)

data FileInputCfg = FileInputCfg
  { _filename  :: FilePath
  , _chunkSize :: Int
  } deriving (Show)

data InputCfg
  = ISoapy SoapySDRInputCfg
  | IFile FileInputCfg
  deriving (Show)

data Opts = Opts
  { _input      :: InputCfg
  , _samplerate :: Double
  , _offset     :: Float
  , _bandwidth  :: Double
  , _numsamples :: Int
  , _outname    :: String
  , _demod      :: Demod
  , _agc        :: Float
  , _channels   :: Int
  , _mix        :: Bool
  } deriving (Show)

parseFileInput :: Parser FileInputCfg
parseFileInput =
  FileInputCfg <$>
  strOption
    (long "filename" <> showDefault <> metavar "NAME" <>
     help "Input (CF32) file name") <*>
  option
    auto
    (long "chunksize" <> help "Chunk size ins CF32 sample" <> showDefault <>
     value 1024 <>
     metavar "INT")

parseSDRInput :: Parser SoapySDRInputCfg
parseSDRInput =
  SoapySDRInputCfg <$>
  strOption
    (long "devname" <> showDefault <> value "rtlsdr" <> metavar "NAME" <>
     help "Soapy device/driver name") <*>
  option
    auto
    (long "frequency" <> short 'f' <> help "Rx frequency in Hz" <> showDefault <>
     value 100.0e6 <>
     metavar "DOUBLE") <*>
  option
    auto
    (long "gain" <> short 'g' <> help "SDR gain level (0 = auto)" <> showDefault <>
     value 0 <>
     metavar "DOUBLE")

parseInput :: Parser InputCfg
parseInput = (IFile <$> parseFileInput) <|> (ISoapy <$> parseSDRInput)

parser :: Parser Opts
parser =
  Opts <$> parseInput <*>
  option
    auto
    (long "samplerate" <> short 's' <> help "Sample rate in Hz" <> showDefault <>
     value 2.56e6 <>
     metavar "DOUBLE") <*>
  option
    auto
    (long "offset" <> help "Offset frequency in Hz" <> showDefault <> value 0.0 <>
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
  option
    auto
    (long "demod" <> help "Demodulation type" <> showDefault <> value DeNo) <*>
  option
    auto
    (long "agc" <> short 'a' <>
     help "Enable AGC with squelch threshold in [dB] (0 = no AGC)" <>
     showDefault <>
     value 0.0 <>
     metavar "DOUBLE") <*>
  option
    auto
    (long "channels" <> short 'c' <>
     help "Number of channels to split the signal into" <>
     showDefault <>
     value 1 <>
     metavar "INT") <*>
  switch
    (long "mix" <> short 'm' <>
     help
       "Instead of outputting separate file for each channel, mix them into one" <>
     showDefault)

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parser <**> helper)
        (fullDesc <>
         progDesc "Process samples from an SDR retrieved via SoapySDR" <>
         header "soapy-sdr")

initSoapySource sr sc = do
  devs <- CS.enumerate
  let dnames = mapMaybe (M.lookup "driver" . M.fromList) devs
      dev = _devname sc
  case length dnames of
    0 -> putStrLn "No SDR devices detected" >> return Nothing
    _ -> do
      putStrLn $ "Available devices: " ++ show dnames
      if dev `elem` dnames
        then do
          putStrLn $ "Using device: " ++ dev
          src <- CS.openSource (_devname sc) sr (_frequency sc) (_gain sc)
          return $ Just (CS.readChunks src, CS.closeSource src)
        else do
          putStrLn $ "Device " ++ dev ++ " not found"
          return Nothing

initFileSource cs fp = do
  eh <- try (CS.openAudioFile fp)
  case eh of
    Left e ->
      case fromException e of
        Just CS.SoapyException -> return Nothing
        Nothing -> return $ Just (CS.readFromFile cs fp, return ())
    Right h -> return $ Just (CS.readFromAudioFile cs h, CS.closeAudioFile h)

sdrProcess :: Opts -> IO ()
sdrProcess opts = do
  mSrc <-
    case _input opts of
      ISoapy c -> initSoapySource (_samplerate opts) c
      IFile c  -> initFileSource (_chunkSize c) (_filename c)
  case mSrc of
    Just (src, csrc) -> do
      let ns = fromIntegral $ _numsamples opts
          getResampler sr bw
            | bw == 0 = id
            | otherwise =
              let resamp_rate = realToFrac (bw / sr)
               in CS.resampler resamp_rate 60.0
          agc =
            if _agc opts /= 0.0
              then CS.automaticGainControl (_agc opts)
              else id
      let resampler = getResampler (_samplerate opts) (_bandwidth opts)
          offset
            | f == 0 = id
            | f > 0 = CS.mixDown f
            | otherwise = CS.mixUp (-f)
            where
              f = 2 * pi * _offset opts / realToFrac (_samplerate opts)
      (process, cleanup) <- CS.unPipe (resampler . offset)
      let prep = CS.takeNArr ns . process
          assembleFold sink demod name nc =
            let sinks =
                  fmap
                    (\n -> CS.addPipe demod (sink $ name ++ "_ch" ++ show n))
                    [1 .. nc]
             in CS.addPipe
                  CS.dcBlocker
                  (if nc > 1
                     then CS.compact (nch * 4 * 1024) $
                          if _mix opts
                            then CS.addPipe
                                   (CS.mix .
                                    CS.mux (replicate nch demod) .
                                    CS.firpfbchChannelizer nc)
                                   (sink name)
                            else CS.addPipe
                                   (CS.firpfbchChannelizer nc)
                                   (CS.distribute_ sinks)
                     else CS.addPipe demod (sink name))
          nch = _channels opts
          outBW =
            if _bandwidth opts == 0
              then _samplerate opts
              else _bandwidth opts
          getAudioSink decim fmt chn =
            let srOut = round outBW `div` decim
             in CS.audioFileSink fmt (srOut `div` nch) (_numsamples opts) chn
          runFold fdl = S.fold fdl (prep src)
      case _demod opts of
        DeNo ->
          runFold
            (assembleFold
               (\n -> CS.fileSink $ n ++ ".cf32")
               agc
               (_outname opts)
               nch)
        DeNBFM kf fmt ->
          runFold
            (assembleFold
               (getAudioSink 1 fmt 1)
               (CS.fmDemodulator kf . agc)
               (_outname opts)
               nch)
        DeWBFM decim fmt ->
          runFold
            (assembleFold
               (getAudioSink decim fmt 1)
               (CS.wbFMDemodulator outBW decim . agc)
               (_outname opts)
               nch)
        DeFMS decim fmt -> do
          let sink = getAudioSink decim fmt 2 (_outname opts)
          dec <- CS.stereoFMDecoder outBW decim sink
          runFold $ CS.addPipe (CS.fmDemodulator 0.8 . agc) dec
        DeAM fmt ->
          runFold
            (assembleFold
               (getAudioSink 1 fmt 1)
               (CS.amDemodulator . agc)
               (_outname opts)
               nch)
      cleanup
      csrc
    Nothing -> putStrLn $ "Unable to open source: " ++ show (_input opts)

run :: Opts -> IO ()
run = sdrProcess
