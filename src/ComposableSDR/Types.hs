{-# LANGUAGE ExistentialQuantification #-}

module ComposableSDR.Types
  ( compose
  , addPipe
  , unPipe
  , Demodulator
  , Pipe(..)
  , ArrayPipe
  ) where

import           Control.Category                           (Category (..))
import           Prelude                                    hiding ((.))

import qualified Control.Monad.Catch                        as MC
import qualified Control.Monad.Trans.Control                as MTC

import qualified Streamly.Internal.Data.Fold.Types          as FL
import           Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import qualified Streamly.Memory.Array                      as A
import qualified Streamly.Prelude                           as S

import           ComposableSDR.Common

data Pipe m a b = forall r. Pipe 
  { _start :: m r
  , _process :: r -> a -> m b
  , _done :: r -> m ()
  }

type ArrayPipe m a b = Pipe m (A.Array a) (A.Array b)
type Demodulator = ArrayPipe IO SamplesIQCF32 Float

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
