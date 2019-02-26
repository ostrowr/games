{-# LANGUAGE BangPatterns #-}
-- import           Data.Int
-- import           Text.Printf
import           Poker
import           Data.List

main :: IO ()
main = do
  let withScores = [(scoreHand x, x) | x <- allHands]
  let sorted = sortBy (\(a, _) (b, _) -> compare a b) withScores
  print $ last sorted
