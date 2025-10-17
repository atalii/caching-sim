{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Data.Cache.Sim.Types where

import Control.Monad.State.Lazy

data Act e = Hit e | Replace (Maybe e) e
  deriving (Show, Eq)

data Handler a e = (Eq e) => Handler (a -> e -> (Act e, a))

data Algorithm a e = (Eq e) => Alg a (Handler a e)

type OnlineAlg a e c = c -> Algorithm a e

instance (Show a) => Show (Algorithm a e) where
  show (Alg state _) = show state

type CachingEnvironmentT e c m a = (Eq e) => StateT (Algorithm c e) m a

request :: (Eq e, Monad m) => e -> CachingEnvironmentT e c m (Act e)
request elem = do
  state <- get
  let (act1, innerState') = stepper state elem
  put $ set state innerState'
  return act1

stepper :: (Eq e) => Algorithm a e -> e -> (Act e, a)
stepper (Alg state (Handler h)) = h state

set :: (Eq e) => Algorithm a e -> a -> Algorithm a e
set (Alg _ handler) a' = Alg a' handler
