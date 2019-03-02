module Masks where

import           Data.Int
import           Data.List                      ( foldl' )
import           Data.Bits

setBits :: [Int] -> Int64
setBits = foldl' setBit zeroBits

numMasks :: [Int64]
numMasks = map setBits [ [x .. x + 3] | x <- [0, 4 .. 51] ]

suitMasks :: [Int64]
suitMasks = map setBits [ [x, x + 4 .. 51] | x <- [0 .. 3] ]

-- To test for a straight, there must be no pairs and the
-- hand & straightMask must have five bits set. Using this
-- instead of a sliding window because we can skip all these
-- checks if there is a pair.
straightMasks :: [Int64]
straightMasks =
  map setBits
    $  [ [x .. x + 19] | x <- [0, 4 .. 51 - 19] ]
    ++ [[48 .. 51] ++ [0 .. 15]] -- ten through ace


ones :: [Int]
ones = [0..18009459]