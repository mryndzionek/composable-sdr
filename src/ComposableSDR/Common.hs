module ComposableSDR.Common
  ( elemSize
  , sampleFormat
  , try
  , bracketIO
  , toArray
  , SamplesIQCF32
  , SoapyException(..)
  , module Control.Monad.State
  , module Foreign.Ptr
  , module Foreign.C.Types
  , module Foreign.Storable
  , module Foreign.ForeignPtr
  ) where

import           Control.Exception                    (throwIO)
import           Control.Monad
import qualified Control.Monad.Catch                  as MC
import           Control.Monad.State

import           Foreign.C.Types
import           Foreign.ForeignPtr                   (castForeignPtr,
                                                       plusForeignPtr,
                                                       withForeignPtr)
import           Foreign.Ptr
import           Foreign.Storable

import qualified Streamly.Internal.Data.Fold.Types    as FL

import           ComposableSDR.Types

try :: (MonadIO m, Num a, Ord a) => m a -> m ()
try a = a >>= (\s -> when (s < 0) $ liftIO $ throwIO SoapyException)

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
