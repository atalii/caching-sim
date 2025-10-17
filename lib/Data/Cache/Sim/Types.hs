{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Data.Cache.Sim.Types where

import Control.Monad.State.Lazy

data Act e = Hit e | Replace (Maybe e) e
  deriving (Eq)

boldArrow :: String
boldArrow = "\x1b[;1m→\x1b[0m"

instance (Show e) => Show (Act e) where
  show (Hit e) = "\x1b[;1m[HIT]\x1b[0m " ++ show e
  show (Replace Nothing e) = "    " ++ boldArrow ++ " " ++ show e
  show (Replace (Just old) e) = "    " ++ boldArrow ++ " " ++ show old ++ " → " ++ show e

isMiss :: Act e -> Bool
isMiss (Hit _) = False
isMiss _ = True

isHit :: Act e -> Bool
isHit = not . isMiss

data Handler a e = (Eq e) => Handler (a -> e -> (Act e, a))

data Algorithm a e = (Eq e) => Alg a (Handler a e)

type OnlineAlg a e c = c -> Algorithm a e

instance (Show a) => Show (Algorithm a e) where
  show (Alg state _) = show state

type CachingEnvironmentT e c m a = (Eq e) => StateT (Algorithm c e) m a

newtype CQueue e = CQueue [Maybe e]
  deriving (Eq)

toList :: CQueue e -> [Maybe e]
toList (CQueue l) = l

instance (Show e) => Show (CQueue e) where
  show (CQueue q) = "〈" ++ expand q ++ "〉"
    where
      expand [] = ""
      expand (Nothing : xs) = "_, " ++ expand xs
      expand (Just x : xs) = show x ++ ", " ++ expand xs

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
