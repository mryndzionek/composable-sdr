module ComposableSDR.Common
  ( elemSize
  , sampleFormat
  , try
  , SamplesIQCF32
  , SoapyException(..)
  , module Control.Monad.State
  , module Foreign.Ptr
  , module Foreign.C.Types
  , module Foreign.Storable
  , module Foreign.ForeignPtr
  ) where

import           Control.Exception     (Exception, throwIO)
import           Control.Monad
import           Control.Monad.State

import           Data.Complex
import           Data.Typeable

import           Foreign.C.Types
import           Foreign.ForeignPtr    (castForeignPtr, plusForeignPtr,
                                        withForeignPtr)
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.C.Types()

data SoapyException =
  SoapyException
  deriving (Show, Typeable)

instance Exception SoapyException

type SamplesIQCF32 = Complex CFloat

elemSize :: Int
elemSize = 8

sampleFormat :: String
sampleFormat = "CF32"

try :: (MonadIO m, Num a, Ord a) => m a -> m ()
try a = a >>= (\s -> when (s < 0) $ liftIO $ throwIO SoapyException)
