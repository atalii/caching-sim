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
      get >>= lift . print
      rq <- lift getReq
      act <- request rq
      let rqs' = rqs ++ [rq]
      lift $ print act
      lift $ print $ fitf 2 rqs'
      repl rqs'

getReq :: IO String
getReq = do
  putStr "rq> "
  _ <- hFlush stdout
  getLine

main :: IO ()
main = evalStateT main' $ s3fifo (2, 4)
