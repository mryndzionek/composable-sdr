module ComposableSDR.Trans
  ( takeNArr
  , compact
  , delay
  , mix
  , mux
  , tee
  , distribute
  , distribute_
  , mapA
  , lMapA
  , foldArray
  ) where

import           Control.Category                           (Category (..))
import           Prelude                                    hiding ((.))

import           Foreign.Storable.Tuple                     ()

import qualified Streamly.Internal.Data.Fold.Types          as FL
import           Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import qualified Streamly.Internal.Memory.Array.Types       as AT (Array (..),
                                                                   newArray,
                                                                   newArray,
                                                                   spliceTwo,
                                                                   splitAt)
import qualified Streamly.Memory.Array                      as A
import qualified Streamly.Prelude                           as S

import           ComposableSDR.Types
import           ComposableSDR.Common

takeNArr ::
     (IsStream t, Storable a, MonadIO m)
  => Int
  -> t m (AT.Array a)
  -> t m (AT.Array a)
takeNArr n = S.map fst . S.takeWhile ((/= 0) . snd) . foldArrL
  where
    foldArrL = S.postscan fld
    fld =
      FL.Fold
        (\(_, l) a -> return $ update a l)
        ((,) <$> liftIO (AT.newArray 0) <*> return 0)
        return
    update a l =
      let togo = n - l
       in if togo == 0
            then (a, 0)
            else if togo >= A.length a
                   then (a, l + A.length a)
                   else (trim a togo, n)
    trim a l =
      if l < A.length a
        then fst $ AT.splitAt l a
        else a

compact ::
     Storable a => Int -> FL.Fold IO (A.Array a) b -> FL.Fold IO (A.Array a) b
compact n (FL.Fold step1 start1 done1) = FL.Fold step start done
  where
    start = do
      s <- start1
      let a0 = A.fromList []
      return (s, a0)
    done (s, b) = do
      s' <- step1 s b
      done1 s'
    step (s, b) a =
      let m = A.length b + A.length a
       in if m >= n
            then do
              ba <- AT.spliceTwo b a
              (a', b') <-
                if m == n
                  then do
                    let a0 = A.fromList []
                    return (ba, a0)
                  else return $ AT.splitAt n ba
              s' <- step1 s a'
              return (s', b')
            else do
              ba <- AT.spliceTwo b a
              return (s, ba)

delay ::
     (MonadIO m, Storable a, Num a)
  => Int
  -> FL.Fold m (A.Array (a, a)) b
  -> FL.Fold m (A.Array a) b
delay n (FL.Fold step1 start1 done1) = FL.Fold step start done
  where
    start = do
      s <- start1
      let a0 = A.fromList (replicate n 0)
      return (s, a0)
    done (s, b) = do
      s' <- step1 s (A.fromList $ zip (A.toList b) (repeat 0))
      done1 s'
    step (s, b) a = do
      ba <- AT.spliceTwo b a
      let (a', b') = AT.splitAt (A.length a - n) ba
      s' <- step1 s (A.fromList $ zip (A.toList a) (A.toList a'))
      return (s', b')

distribute :: MonadIO m => [FL.Fold m a b] -> FL.Fold m [a] [b]
distribute fs = FL.Fold step initial extract
  where
    initial =
      mapM (\(FL.Fold s i e) -> i >>= \r -> return (FL.Fold s (return r) e)) fs
    step ss as = do
      zipWithM_ (\(FL.Fold s i _) a -> i >>= \r -> void (s r a)) ss as
      return ss
    extract = mapM (\(FL.Fold _ i e) -> i >>= \r -> e r)

distribute_ :: MonadIO m => [FL.Fold m a b] -> FL.Fold m [a] ()
distribute_ = void . distribute

mix :: (MonadIO m, Num a, Storable a) => Pipe m [A.Array a] (A.Array a)
mix =
  let f a1 a2 = A.fromList $ zipWith (+) (A.toList a1) (A.toList a2)
   in foldl1 f <$> Control.Category.id

mux :: Monad m => [Pipe m a b] -> Pipe m [a] [b]
mux ps = Pipe start process done
  where
    start = mapM (\(Pipe s p d) -> s >>= \r -> return (Pipe (return r) p d)) ps
    process = zipWithM (\(Pipe s p _) a -> s >>= \r -> p r a)
    done = mapM_ (\(Pipe s _ d) -> s >>= \r -> d r)

tee ::
     (Monad m, Storable b, Storable c)
  => ArrayPipe m a b
  -> ArrayPipe m a c
  -> ArrayPipe m a (b, c)
tee (Pipe start1 process1 done1) (Pipe start2 process2 done2) =
  Pipe start process done
  where
    start = (,) <$> start1 <*> start2
    done (r1, r2) = done2 r2 >> done1 r1
    process (r1, r2) a = do
      b <- process1 r1 a
      c <- process2 r2 a
      return $ A.fromList $ zip (A.toList b) (A.toList c)

mapA :: (Storable a, Storable b) => (a -> b) -> A.Array a -> A.Array b
mapA f = A.fromList . fmap f . A.toList

lMapA :: (Storable a, Storable t) => (t -> a) -> ArrayPipe m a b -> ArrayPipe m t b
lMapA f (Pipe start process done) = Pipe start proc done
  where
    proc r a = process r (mapA f a)

foldArray :: Storable a => (A.Array a -> IO b) -> FL.Fold IO (A.Array a) b
foldArray = FL.Fold AT.spliceTwo (AT.newArray 0)