module Data.Cache.Sim.Attacks (s3fifoAttack) where

import Data.Cache.Sim.Algs (S3FifoConfig)

union :: (Eq e) => e -> [e] -> [e]
union e es | e `elem` es = es
union e es = e : es

-- An element is a positive ℕ.
newtype Element = Element Int
  deriving (Eq)

instance Show Element where
  show (Element x)
    | x <= 26 && x >= 1 =
        show $
          ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'] !! (x - 1)
  show (Element x) = show x

s3fifoAttack :: S3FifoConfig -> [Element]
s3fifoAttack (s, m) =
  let startup = map Element [1 .. s]
      fill = interleaveMisses startup $ map Element ([s + 1 .. (s + m + 1)] ++ cycle [1 .. (s + m + 1)])
   in startup ++ fill ++ [Element (s + m + 1)]
  where
    interleaveMisses _ [] = []
    interleaveMisses (p : ps) (e : es) = [e, p] ++ interleaveMisses (ps ++ [e]) es
