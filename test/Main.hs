module Main where

import Control.Monad ((>=>))
import Control.Monad.State.Lazy (evalStateT, lift)
import Data.Cache.Sim.Algs (lru)
import Data.Cache.Sim.Types
  ( Act (..),
    OnlineAlg,
    isMiss,
    request,
    set,
    stepper,
  )
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "LRU" $ do
    context "when k = 2" $ do
      let lruInstance = (lru :: OnlineAlg [Maybe Char] Char Int) 2

      it "experiences compulsory misses" $ flip evalStateT lruInstance $ do
        act <- request 'A'
        lift $ act `shouldBe` Replace Nothing 'A'
        act <- request 'B'
        lift $ act `shouldBe` Replace Nothing 'B'

      it "consistent misses on a repeated k + 1 scan" $ example $ flip evalStateT lruInstance $ do
        let rqSeq = take 100000 $ cycle ['A', 'B', 'C']
        mapM_ (request >=> (lift . flip shouldSatisfy isMiss)) rqSeq
