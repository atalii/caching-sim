{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Data.List.Extra (snoc)
import System.IO

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

-- Can this be state-monad'd?
set :: (Eq e) => Algorithm a e -> a -> Algorithm a e
set (Alg _ handler) a' = Alg a' handler

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
main = main' $ lru 6
  where
    main' alg = do
      print alg
      (act, state') <- getReq alg
      print act
      main' $ set alg state'

    getReq cache = do
      putStr "rq> "
      _ <- hFlush stdout
      stepper cache <$> getLine
