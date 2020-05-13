module ComposableSDR.Sink
  ( fileSink
  , stdOutSink
  , audioFileSink
  , AudioFormat(..)
  ) where

import           System.IO                            (stdout)

import qualified Control.Monad.Catch                  as MC

import qualified Sound.File.Sndfile                   as SF

import qualified Streamly.Internal.Data.Fold          as F
import qualified Streamly.Internal.Data.Fold.Types    as FL
import qualified Streamly.Internal.FileSystem.File    as FS
import qualified Streamly.Internal.FileSystem.Handle  as FH
import qualified Streamly.Memory.Array                as A

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
