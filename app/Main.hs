{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Cache.Sim.Algs (fitf)
import Data.Cache.Sim.Attacks (s3fifoAttack)

main :: IO ()
main = mapM_ runOn configs
  where
    configs = concatMap genUpTo [1 .. 100]
    l = 1000000
    runOn conf@(s, m) =
      let attack = take l $ s3fifoAttack conf
          cost :: Double = fromIntegral l / fromIntegral (snd (fitf (s + m) attack))
       in putStrLn $ show s <> "," <> show m <> "," <> show cost

    genUpTo n = map (,n) [1 .. n]
