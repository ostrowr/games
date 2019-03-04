-- intended to be piped into a file; output will be
-- several gigabytes

{-# LANGUAGE ScopedTypeVariables #-}

module PrintMappings where

import           Poker (scoreHand, sevenToBestFive)
import Text.Printf (printf)

main :: IO ()
main = do
  let scores = map (\x -> scoreHand (snd x)) sevenToBestFive
  let sevenToBestFiveWithScores = zip scores sevenToBestFive
  -- score, seven-card hand, five-card hand
  let formatted = map (\x -> printf "%d\t%d\t%d" (fst x) (fst (snd x)) (snd (snd x))) sevenToBestFiveWithScores
  mapM_ putStrLn formatted
