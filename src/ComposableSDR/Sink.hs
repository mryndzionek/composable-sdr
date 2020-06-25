module ComposableSDR.Sink
  ( fileSink
  , stdOutSink
  , audioFileSink
  , constellationPlotSink
  , AudioFormat(..)
  ) where

import           System.FilePath.Posix               (takeBaseName)
import           System.IO                           (stdout)

import           Data.Complex                        (imagPart, realPart)

import           Text.Printf

import qualified Control.Monad.Catch                 as MC

import qualified Sound.File.Sndfile                  as SF

import qualified Streamly.Internal.Data.Fold         as F
import qualified Streamly.Internal.Data.Fold.Types   as FL
import qualified Streamly.Internal.FileSystem.File   as FS
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Memory.Array               as A

import           ComposableSDR.Common
import           ComposableSDR.Types

{-# INLINABLE fileSink #-}
fileSink ::
     (MonadIO m, MC.MonadCatch m, Storable a)
  => FilePath
  -> FL.Fold m (A.Array a) ()
fileSink = FS.writeChunks

{-# INLINABLE stdOutSink #-}
stdOutSink ::
     (MonadIO m, MC.MonadCatch m, Storable a) => FL.Fold m (A.Array a) ()
stdOutSink = FH.writeChunks stdout

openAudioFile :: AudioFormat -> FilePath -> Int -> Int -> Int -> IO SF.Handle
openAudioFile fmt fp sr sn nch = SF.openFile (fp ++ ext) SF.WriteMode info
  where
    sfFmt AU  = (SF.HeaderFormatAu, ".au")
    sfFmt WAV = (SF.HeaderFormatWav, ".wav")
    (sff, ext) = sfFmt fmt
    info =
      SF.Info
        sn
        sr
        nch
        (SF.Format sff SF.SampleFormatFloat SF.EndianBig)
        1
        False

writeToAudioFile :: (MonadIO m) => SF.Handle -> A.Array Float -> m ()
writeToAudioFile h a = do
  _ <- liftIO $ SF.hPutBuffer h (toArray a)
  return ()

closeAudioFile :: SF.Handle -> IO ()
closeAudioFile = SF.hClose

audioFileSink ::
     (MC.MonadCatch m, MonadIO m)
  => AudioFormat
  -> Int
  -> Int
  -> Int
  -> String
  -> FL.Fold m (A.Array Float) ()
audioFileSink fmt sr sn nch fp =
  let wav = openAudioFile fmt fp sr sn nch
   in bracketIO wav closeAudioFile (F.drainBy . writeToAudioFile)

constellationPlotSink ::
     (MC.MonadCatch m, MonadIO m)
  => FilePath
  -> FL.Fold m (A.Array SamplesIQCF32) ()
constellationPlotSink fp =
  let writeHeader = writeFile fp $ unlines ["clear all; close all;", "v = [];"]
      toFloat (CFloat f) = f
      writeBody as =
        liftIO $
        appendFile fp $
        unlines $
          (\s ->
             printf
               "v(end+1) = %12.4e + j*%12.4e;"
               (toFloat $ realPart s)
               (toFloat $ imagPart s)) <$>
        A.toList as
      writeFooter =
        appendFile fp $
        unlines
          [ "n = length(v);"
          , "figure('color','white','position',[100 100 1200 400]);"
          , "plot(real(v), imag(v), 'x', 'Color',[0 0.2 0.4]);"
          , "xlabel('In-Phase');"
          , "ylabel('Quadrature');"
          , "grid on;"
          , "print -dpng -color \"-S1200,600\" " ++ takeBaseName fp ++ ".png"
          ]
   in bracketIO writeHeader (const writeFooter) (F.drainBy . const writeBody)
