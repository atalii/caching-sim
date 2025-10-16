module Main where

import Data.Cache.Sim.Algs (fitf, s3fifo)
import Data.Cache.Sim.Types (set, stepper)
import System.IO

main :: IO ()
main = main' [] $ s3fifo (2, 4)
  where
    main' rSeq alg = do
      print alg
      (rSeq', act, state') <- getReq rSeq alg
      print act
      print $ fitf 2 rSeq'
      main' rSeq' $ set alg state'

    getReq rSeq cache = do
      putStr "rq> "
      _ <- hFlush stdout
      rq <- getLine
      let (action, cache') = stepper cache rq
      return (rSeq ++ [rq], action, cache')
