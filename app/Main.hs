{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Data.List.Extra (snoc)
import System.IO

-- my notation. #mynotation
(×) :: a -> Int -> [a]
(×) = flip replicate

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

-- Construct an S3FIFO algorithm instance. We take the size of the small and main queues. The ghost queue is taken to be the same size as the main queue.
s3fifo :: (Eq e) => OnlineAlg ([Maybe (e, Int)], [Maybe (e, Int)], [Maybe e]) e (Int, Int)
s3fifo (s, m) = Alg (Nothing × s, Nothing × m, Nothing × m) (Handler handler)
  where
    handler st@(sq, mq, gq) rq =
      let (sq', sqFound) = searchIn sq rq
          (mq', mqFound) = searchIn mq rq
       in if sqFound || mqFound
            then (Hit rq, (sq', mq', gq))
            else insert st rq

    searchIn [] _ = ([], False)
    searchIn (Just (x, c) : xs) e
      | x == e = (Just (x, (c + 1) `min` 3) : xs, True)
    searchIn (x : xs) e = let (q, r) = searchIn xs e in (x : q, r)

    insert st@(sh : sx, mq, gq@(_ : gqs)) rq =
      if Just rq `elem` gq
        then
          let (act, (sq', mq', _)) = insertM st rq
           in (act, (sq', mq', gq `without` Just rq))
        else case sh of
          Nothing -> (Replace Nothing rq, (sx ++ [Just (rq, 0)], mq, gq))
          -- If the FIFO'd element of the small queue hasn't been used, put it on the ghost queue and add the new element.
          Just (shV, 0) -> (Replace (Just shV) rq, (sx ++ [Just (rq, 0)], mq, gqs ++ [Just shV]))
          -- Otherwise, put reolace it with rq and put it in the main queue.
          Just (shV, _) -> insertM (sx ++ [Just (rq, 0)], mq, gq) shV
    insert _ _ = error "s, m must be > 0"

    insertM :: ([Maybe (e, Int)], [Maybe (e, Int)], [Maybe e]) -> e -> (Act e, ([Maybe (e, Int)], [Maybe (e, Int)], [Maybe e]))
    insertM (sq, Just (mV, mCount) : ms, gq) rq
      | mCount > 0 = insertM (sq, ms ++ [Just (mV, mCount - 1)], gq) rq
      | otherwise = (Replace (Just mV) rq, (sq, Just (rq, 0) : ms, gq))
    insertM (sq, Nothing : ms, gq) rq = (Replace Nothing rq, (sq, ms ++ [Just (rq, 0)], gq))
    insertM _ _ = error "m must be > 0"

lru :: (Eq e) => OnlineAlg [Maybe e] e Int
lru k = Alg (Nothing × k) (Handler handler)
  where
    handler state@(victim : used) rq =
      if Just rq `elem` state
        then (Hit rq, reInsert rq state)
        else (Replace victim rq, used `snoc` Just rq)
    handler _ _ = error "k must be > 0"

    reInsert rq state = (state `without` Just rq) `snoc` Just rq

without :: (Eq a) => [a] -> a -> [a]
without [] _ = []
without (x : xs) t | t == x = xs
without (x : xs) t = x : without xs t

main :: IO ()
main = main' $ s3fifo (3, 6)
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
