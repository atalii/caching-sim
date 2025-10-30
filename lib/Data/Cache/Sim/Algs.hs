module Data.Cache.Sim.Algs (lru, s3fifo, fitf) where

import Data.Cache.Sim.Types
import Data.List.Extra (snoc)
import Data.Maybe (catMaybes)

-- my notation. #mynotation
(×) :: a -> Int -> [a]
(×) = flip replicate

-- Construct an S3FIFO algorithm instance. We take the size of the small and main queues. The ghost queue is taken to be the same size as the main queue.
s3fifo :: (Eq e) => OnlineAlg (CQueue (e, Int), CQueue (e, Int), CQueue e) e (Int, Int)
s3fifo (s, m) = Alg (CQueue (Nothing × s), CQueue (Nothing × m), CQueue (Nothing × m)) (Handler handler)
  where
    handler st@(CQueue sq, CQueue mq, CQueue gq) rq =
      let (sq', sqFound) = searchIn sq rq
          (mq', mqFound) = searchIn mq rq
       in if sqFound || mqFound
            then (Hit rq, (CQueue sq', CQueue mq', CQueue gq))
            else insert st rq

    searchIn [] _ = ([], False)
    searchIn (Just (x, c) : xs) e
      | x == e = (Just (x, (c + 1) `min` 3) : xs, True)
    searchIn (x : xs) e = let (q, r) = searchIn xs e in (x : q, r)

    insert st@(CQueue (sh : sx), CQueue mq, CQueue gq@(_ : gqs)) rq =
      if Just rq `elem` gq
        then insertM st rq
        else case sh of
          Nothing -> (Replace Nothing rq, (CQueue $ sx ++ [Just (rq, 0)], CQueue mq, CQueue gq))
          -- If the FIFO'd element of the small queue hasn't been used, put it on the ghost queue and add the new element.
          Just (shV, 0) -> (Replace (Just shV) rq, (CQueue $ sx ++ [Just (rq, 0)], CQueue mq, CQueue $ gqs ++ [Just shV]))
          -- Otherwise, put reolace it with rq and put it in the main queue.
          Just (shV, _) -> insertM (CQueue $ sx ++ [Just (rq, 0)], CQueue mq, CQueue gq) shV
    insert _ _ = error "s, m must be > 0"

    insertM :: (CQueue (e, Int), CQueue (e, Int), CQueue e) -> e -> (Act e, (CQueue (e, Int), CQueue (e, Int), CQueue e))
    insertM (sq, CQueue (Just (mV, mCount) : ms), gq) rq
      | mCount > 0 = insertM (sq, CQueue $ ms ++ [Just (mV, mCount - 1)], gq) rq
      | otherwise = (Replace (Just mV) rq, (sq, CQueue $ ms ++ [Just (rq, 0)], gq))
    insertM (sq, CQueue (Nothing : ms), gq) rq = (Replace Nothing rq, (sq, CQueue $ ms ++ [Just (rq, 0)], gq))
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

type Cost = Int

mapFirst :: (a -> c) -> (a, b) -> (c, b)
mapFirst f (a, b) = (f a, b)

-- Run farthest in the future on a given request sequence with cache size `h`.
fitf :: (Eq e) => Int -> [e] -> ([CQueue e], Cost)
fitf h = mapFirst (map CQueue) . run' (Nothing × h)
  where
    run' :: (Eq e) => [Maybe e] -> [e] -> ([[Maybe e]], Cost)
    run' _ [] = ([], 0)
    run' cache (r : rs) | Just r `elem` cache = let (future, cost) = run' cache rs in (cache : future, cost)
    run' cache (r : rs)
      | Nothing `elem` cache =
          let replaced = Just r : cache `without` Nothing
              (future, cost) = run' replaced rs
           in (replaced : future, cost + 1)
    run' cache (r : rs) =
      let victim = findFarthest (catMaybes cache) rs
          replaced = Just r : cache `without` Just victim
          (future, cost) = run' replaced rs
       in (replaced : future, cost + 1)

    findFarthest :: (Eq e) => [e] -> [e] -> e
    findFarthest [x] _ = x
    findFarthest [] _ = error "h must be > 0"
    findFarthest (x : _) [] = x -- arbtirary tie breaking
    findFarthest cache (x : xs) = if x `elem` cache then findFarthest (cache `without` x) xs else findFarthest cache xs

without :: (Eq a) => [a] -> a -> [a]
without [] _ = []
without (x : xs) t | t == x = xs
without (x : xs) t = x : without xs t
