{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module ComposableSDR.Types
  ( compose
  , addPipe
  , unPipe
  , toArray
  , elemSize
  , sampleFormat
  , Demodulator
  , Pipe(..)
  , ArrayPipe
  , AudioFormat(..)
  , SamplesIQCF32
  , SoapyException(..)
  , Array(..)
  ) where

import           Control.Category                           (Category (..))
import           Prelude                                    hiding ((.))

import           Data.Complex
import           Data.Typeable

import           Foreign.C.Types
import           Foreign.ForeignPtr                         (plusForeignPtr,
                                                             withForeignPtr)
import           Foreign.Ptr

import           Control.Exception                          (Exception)
import           Control.Monad
import qualified Control.Monad.Catch                        as MC
import           Control.Monad.State
import qualified Control.Monad.Trans.Control                as MTC

import qualified Sound.File.Sndfile                         as SF

import qualified Streamly.Internal.Data.Fold.Types          as FL
import           Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import qualified Streamly.Internal.Memory.Array.Types       as AT (Array (..))
import qualified Streamly.Memory.Array                      as A
import qualified Streamly.Prelude                           as S

data AudioFormat
  = AU
  | WAV
  deriving (Show, Read)

data Pipe m a b = forall r. Pipe 
  { _start :: m r
  , _process :: r -> a -> m b
  , _done :: r -> m ()
  }

newtype Array a = Array
  { fromArray :: AT.Array a
  }

instance SF.Buffer Array Float where
  fromForeignPtr fp i n =
    withForeignPtr fp $ \p -> do
      let v =
            AT.Array
              { AT.aStart = fp `plusForeignPtr` i
              , AT.aEnd = p `plusPtr` (i + 4 * n)
              , AT.aBound = p `plusPtr` (i + 4 * n)
              }
      return $ toArray v
  toForeignPtr = return . (\x -> (AT.aStart x, 0, A.length x)) . fromArray

type ArrayPipe m a b = Pipe m (A.Array a) (A.Array b)
type Demodulator = ArrayPipe IO SamplesIQCF32 Float

data SoapyException =
  SoapyException
  deriving (Show, Typeable)

instance Exception SoapyException

type SamplesIQCF32 = Complex CFloat

elemSize :: Int
elemSize = 8

sampleFormat :: String
sampleFormat = "CF32"

toArray :: AT.Array a -> Array a
toArray = Array

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

unPipe :: (IsStream t, MonadIO m,
          MTC.MonadBaseControl IO m,
          MC.MonadThrow m) =>
          Pipe m a b -> m (t m a -> t m b, m ())
unPipe (Pipe creat process dest) = do
  r <- creat
  return (S.mapM (process r), dest r)

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
