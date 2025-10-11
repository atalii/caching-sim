{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Data.List.Extra (snoc)

data Act e = Hit e | Replace (Maybe e) e
  deriving (Show)

data Handler a e = (Eq e) => Handler (a -> e -> (Act e, a))

data Algorithm a e = (Eq e) => Alg a (Handler a e)

type OnlineAlg a e c = c -> Algorithm a e

data OfflineAlg a e c = (Eq e) => OfflineAlg (c -> [e] -> Algorithm a e)

instance (Show a) => Show (Algorithm a e) where
  show (Alg state _) = show state

stepper :: (Eq e) => Algorithm a e -> e -> (Act e, a)
stepper (Alg state (Handler h)) = h state

lru :: (Eq e) => OnlineAlg [Maybe e] e Int
lru k = Alg (k `replicate` Nothing) (Handler handler)
  where
    handler state@(victim : used) rq =
      if Just rq `elem` state
        then (Hit rq, reInsert rq state)
        else (Replace victim rq, used `snoc` Just rq)
    handler _ _ = error "k must be > 0"

    reInsert rq state = (state `without` Just rq) `snoc` Just rq

    without [] _ = []
    without (x : xs) t | t == x = xs
    without (x : xs) t = x : without xs t

main :: IO ()
main =
  let lru' = (lru 6 :: Algorithm [Maybe Char] Char)
      (act, lru'') = stepper lru' 'A'
   in do
        print act
        print lru''
