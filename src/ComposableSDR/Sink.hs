{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import qualified Streamly.Internal.Memory.Array.Types as AT (Array (..))
import qualified Streamly.Memory.Array                as A

import ComposableSDR.Common

data AudioFormat
  = AU
  | WAV
  deriving (Show, Read)

newtype Array a = Array
  { fromArray :: AT.Array a
  }

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

toArray :: AT.Array a -> Array a
toArray = Array

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
