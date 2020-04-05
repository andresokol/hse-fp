{-# LANGUAGE Arrows #-}

module Lib where

import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Prelude          hiding (id, (.))

-- --- --
--     --
--  1  --
--     --
-- --- --

-- 1.1
newtype Reader r a = Reader { runReader :: r -> a }

-- 1.1.1
instance Functor (Reader r) where
    fmap f x = Reader (\w -> f $ runReader x w)
instance Applicative (Reader r) where
    pure a = Reader $ \w -> a
    Reader r <*> Reader d = Reader $ \w -> r w (d w)
instance Monad (Reader r) where
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

-- 1.1.2

ask :: Reader r r
ask = Reader $ \w -> w

local
  :: (r -> r)
  -> Reader r a
  -> Reader r a
local f a = Reader $ \w -> runReader a (f w)

-- 1.2
newtype Writer w a
  = Writer { runWriter :: (a, w) }

-- 1.2.1
instance Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    Writer (a, w) <*> Writer (b, w') = Writer (a b, w `mappend` w')

instance Monoid w => Monad (Writer w) where
    return x = Writer (x, mempty)
    Writer (a, w) >>= m = do
      let (b, w') = runWriter (m a)
      Writer (b, w `mappend` w')

-- 1.2.2
tell
  :: Monoid w
  => w
  -> Writer w ()
tell w = Writer ((), w)

listen
  :: Monoid w
  => Writer w a
  -> Writer w (w, a)
listen (Writer (a, w)) = Writer ((w, a), w)

pass
  :: Monoid w
  => Writer w (a, w -> w)
  -> Writer w a
pass (Writer ((a, f), w)) = Writer (a, f w)

-- 1.3
newtype State s a
  = State { runState :: s -> (a, s) }

-- 1.3.1
get :: State s s
get = State $ \w -> (w, w)

put :: s -> State s ()
put = \s -> State $ const ((), s)

-- 1.3.2
instance Functor (State s) where
    fmap f a = State $ \s -> let (a', s') = runState a s 
                             in (f a', s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    f <*> a = State $ \s -> let (a', s')  = runState a s
                                (f', s'') = runState f s'
                            in (f' a', s'')

instance Monad (State s) where
    m >>= k = State $ \s -> case runState m s of (m', s') -> runState (k m') s'

-- --- --
--     --
--  2  --
--     --
-- --- --

-- 2.1
newtype SignalFunction a b = SF ((a, Double) -> (SignalFunction a b, b))

instance Category SignalFunction where
    id = SF $ \(i, _) -> (id, i)
    SF f . SF g = SF $ \(i, t) -> let (sf_g, result_g) = g (i, t)
                                      (sf_f, result_f) = f (result_g, t)
                                  in (sf_f . sf_g, result_f)

instance Arrow SignalFunction where
    arr f = SF $ \(i, _) -> (arr f, f i)
    first (SF f) = SF $ \((a, b), t) -> let (sf_f, result) = f (a, t)
                                        in (first sf_f, (result, b))

-- 2.2
integral :: SignalFunction Double Double
integral = iter 0.0 0.0
    where
        iter :: Double -> Double -> SignalFunction Double Double
        iter acc l = SF $ \(r, t) -> let acc' = (l + r) * t / 2 + acc
                                      in (iter acc' r, acc')

-- 2.3
someFunction :: SignalFunction (Double, Double) (Double, Double)
someFunction = proc (x, y) -> do
    x2  <- arr (*2) -< x
    y3  <- arr (*3) -< y
    res <- arr (uncurry (+)) -< (x2, y3)

    i <- integral -< res
    returnA -< (i, res)

-- --- --
--     --
--  3  --
--     --
-- --- --

-- --- --
--     --
--  4  --
--     --
-- --- --

newtype MyCont r a
  = MyCont { runCont :: (a -> r) -> r }

-- 4.1
instance Functor (MyCont r) where
    fmap f a = MyCont $ \r -> runCont a $ r . f

-- 4.2
instance Applicative (MyCont r) where
    pure a = MyCont $ \r -> r a
    f <*> a = MyCont $ \r -> runCont f $ \r' -> runCont a $ r . r'

-- 4.3
instance Monad (MyCont r) where
    a >>= f = MyCont $ \r -> runCont a $ \r' -> runCont (f r') r

-- --- --
--     --
--  5  --
--     --
-- --- --

class MonadTrans n where
  lift :: Monad m => m a -> n m a

-- 5.1
newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
    lift m = MaybeT $ fmap return m

-- 5.2
newtype ContT r m a
  = ContT { runContT :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

-- 5.3
newtype StateT s m a
  = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> do
        a <- ma
        return (a, s)
