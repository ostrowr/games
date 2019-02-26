{-# LANGUAGE ScopedTypeVariables #-}

module Poker where

import           Data.Int
import           Masks
import           Data.Bits
import           Data.List                      ( tails, foldl' )

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [ y : ys | y : xs' <- tails xs, ys <- combinations (n - 1) xs' ]

allHands :: [Int64]
allHands = map setBits $ combinations 5 [0 .. 51]

powersOfFour :: [Int]
powersOfFour = [ 4 ^ x | x <- [0 :: Int ..] ]

-- scoreHand a > scoreHand b iff a beats b
scoreHand :: Int64 -> Int64
scoreHand hand = shiftL 26 handType .|. kickerScore
 where
  numberCounts    = map (\mask -> popCount (hand .&. mask)) Masks.numMasks
  suitCounts      = map (\mask -> popCount (hand .&. mask)) Masks.suitMasks
  straightCounts  = map (\mask -> popCount (hand .&. mask)) Masks.straightMasks
  pairs           = length $ filter (== 2) numberCounts
  isTrip          = elem 3 numberCounts
  isQuad          = pairs == 0 && not isTrip && elem 4 numberCounts
  isMaybeFlush    = elem 5 suitCounts -- only need to calculate suitCounts/straightCounts when no pairs, trips, or quads
  isMaybeStraight = elem 5 straightCounts
  isStraight      = pairs == 0 && not isTrip && not isQuad && isMaybeStraight
  isFlush         = pairs == 0 && not isTrip && not isQuad && isMaybeFlush
  handType | isStraight && isFlush = 8
           | isQuad                = 7
           | pairs == 1 && isTrip  = 6
           | isFlush               = 5
           | isStraight            = 4
           | isTrip                = 3
           | pairs == 2            = 2
           | pairs == 1            = 1
           | otherwise             = 0
  -- the number of twos is encoded in the least significant 2
  -- bits, the number of threes in the next 2 bits, and so on.
  numberCountMasks = zipWith shiftL [0::Int64, 2..] numberCounts
  kickerScore = foldl' (.|.) zeroBits numberCountMasks

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Suit = Spades | Clubs | Hearts | Diamonds deriving (Show, Read, Enum)

data Card = Card {val :: CardValue, suit :: Suit} deriving (Show)

fromPrettyHand :: [Card] -> Int64
fromPrettyHand = setBits . map fromCard
  where fromCard c = fromEnum (val c) * 4 + fromEnum (suit c)

toPrettyHand :: Int64 -> [Card]
toPrettyHand = map toCard . getSetBits
 where
  getSetBits h = map fst (filter snd (zip [0 ..] (map (testBit h) [0 .. 51])))
  toCard cardIndex =
    let (v, s) = divMod cardIndex 4 in Card (toEnum v) (toEnum s)

test :: Bool
test = all id (map doTest allHands)
  where doTest hand = fromPrettyHand (toPrettyHand hand) == hand

