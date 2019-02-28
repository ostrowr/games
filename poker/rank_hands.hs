import           Poker

main :: IO ()
main = do
  let x = sevenToBestFive
  let y = sortedHands
  print $ length x
  print $ length y