module Main where

import Control.Monad ((>=>))
import Control.Monad.State.Lazy (evalStateT, lift)
import Data.Cache.Sim.Algs (fitf, lru, s3fifo)
import Data.Cache.Sim.Types
  ( Act (..),
    OnlineAlg,
    isHit,
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

      it "consistently misses on a repeated k + 1 scan" $ example $ flip evalStateT lruInstance $ do
        let rqSeq = take 100000 $ cycle ['A', 'B', 'C']
        mapM_ (request >=> (lift . flip shouldSatisfy isMiss)) rqSeq

  describe "S3FIFO" $ do
    context "when 𝓢 = 2, 𝓜  = 4" $ do
      let s3fifoInstance = s3fifo (2, 4)
      it "handles garbage appropriately" $ example $ flip evalStateT s3fifoInstance $ do
        request 'A'
        request 'B'
        request 'C'
        act <- request 'A'
        lift $ act `shouldSatisfy` isMiss

        request 'D'
        request 'E'
        act <- request 'A'
        -- We expect that we've moved A to 𝓜, and therefore that it survived introducing two new garbage requests (D and E)...
        lift $ act `shouldSatisfy` isHit

  describe "FITF" $ do
    context "when k = 2" $ do
      it "makes eviction decisions offline" $ do
        fitf 2 ['A', 'B', 'C', 'B']
          `shouldNotBe` fitf 2 ['A', 'B', 'C', 'A']
