{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import Control.Monad.State.Lazy (evalStateT, get, lift)
import Data.Cache.Sim.Algs (fitf, s3fifo)
import Data.Cache.Sim.Types (CachingEnvironmentT, request)
import System.IO (hFlush, stdout)

main' :: (Show c) => CachingEnvironmentT String c IO ()
main' = repl []
  where
    repl rqs = do
      lift $ putStr "[\x1b[33;1mA  \x1b[0m] "
      get >>= lift . print

      let (optSeq, cost) = fitf 6 rqs
      lift $ putStr "[\x1b[32;1mOPT\x1b[0m] "
      lift $ mapM_ (putStrLn . ('\t' :) . show) optSeq
      lift $ print cost

      rq <- lift getReq
      act <- request rq
      let rqs' = rqs ++ [rq]
      lift $ print act

      repl rqs'

getReq :: IO String
getReq = do
  putStr "rq> "
  _ <- hFlush stdout
  getLine

main :: IO ()
main = evalStateT main' $ s3fifo (2, 4)
