module Main where

import Data.Cache.Sim.Algs (lru)
import Data.Cache.Sim.Types
  ( Act (..),
    OnlineAlg,
    set,
    stepper,
  )
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "LRU" $ do
    context "when k = 2" $ do
      let lruInstance = (lru :: OnlineAlg [Maybe Char] Char Int) 2
      it "experiences compulsory misses" $ do
        let (act1, s1) = stepper lruInstance 'A'
        act1 `shouldBe` Replace Nothing 'A'
        let lruInstance' = set lruInstance s1
        let (act2, s2) = stepper lruInstance' 'B'
        act2 `shouldBe` Replace Nothing 'B'
