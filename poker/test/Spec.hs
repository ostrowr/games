import Poker

main :: IO ()
main = print test

test :: Bool
test = all id (map doTest allHands)
  where doTest hand = fromPrettyHand (toPrettyHand hand) == hand