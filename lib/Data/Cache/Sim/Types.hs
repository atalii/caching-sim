{-# LANGUAGE ExistentialQuantification #-}

module Data.Cache.Sim.Types where

data Act e = Hit e | Replace (Maybe e) e
  deriving (Show)

data Handler a e = (Eq e) => Handler (a -> e -> (Act e, a))

data Algorithm a e = (Eq e) => Alg a (Handler a e)

type OnlineAlg a e c = c -> Algorithm a e

instance (Show a) => Show (Algorithm a e) where
  show (Alg state _) = show state

stepper :: (Eq e) => Algorithm a e -> e -> (Act e, a)
stepper (Alg state (Handler h)) = h state

-- Can this be state-monad'd?
set :: (Eq e) => Algorithm a e -> a -> Algorithm a e
set (Alg _ handler) a' = Alg a' handler
