{-# LANGUAGE ScopedTypeVariables #-}

module Poker where

import           Data.Int
import           Masks
import           Data.Bits
import           Data.List                      ( tails, foldl', sortBy )
import Data.Set (Set, fromDistinctAscList)
import Data.IntSet (IntSet, fromList, size)
import Data.Array.IArray

type Hand = Int64

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [ y : ys | y : xs' <- tails xs, ys <- combinations (n - 1) xs' ]

allHands :: [Hand]
allHands = map setBits $ combinations 5 [0 .. 51]

-- Returns the hand as an integer, the best 5-card hand
-- from that hand
bestSubsetHand :: [Int] -> (Hand, Hand)
bestSubsetHand cards
  | length cards < 5 = error "Attempted to get hand subset from fewer than 5 cards"
  | otherwise = (bitwiseCards, bestHand) where
      bitwiseCards = setBits cards
      subsetHands = map setBits (combinations 5 cards)
      scores = map scoreHand subsetHands
      scoresWithHands = zip scores subsetHands
      (_, bestHand) = maximum scoresWithHands

sevenCardCombos :: [[Int]]
sevenCardCombos = combinations 7 [0..51]

sevenToBestFive :: [(Hand, Hand)]
sevenToBestFive = map bestSubsetHand sevenCardCombos

-- scoreHand a > scoreHand b iff a beats b
scoreHand :: Hand -> Int32
scoreHand hand = (shiftL handType 28) .|. (fromIntegral kickerScore)
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
  handType | isStraight && isFlush = 8::Int32
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
  numberCountMasks = zipWith shiftL numberCounts [0, 2..]
  kickerScore = foldl' (.|.) zeroBits numberCountMasks

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Suit = Spades | Clubs | Hearts | Diamonds deriving (Show, Read, Enum)

data Card = Card {val :: CardValue, suit :: Suit} deriving (Show)

fromPrettyHand :: [Card] -> Hand
fromPrettyHand = setBits . map fromCard
  where fromCard c = fromEnum (val c) * 4 + fromEnum (suit c)

toPrettyHand :: Hand -> [Card]
toPrettyHand = map toCard . getSetBits
 where
  getSetBits h = map fst (filter snd (zip [0 ..] (map (testBit h) [0 .. 51])))
  toCard cardIndex =
    let (v, s) = divMod cardIndex 4 in Card (toEnum v) (toEnum s)


sortedHands :: [(Hand, Int32)]
sortedHands = let withScores = [(x, scoreHand x) | x <- allHands]
              in sortBy (\(_, a) (_, b) -> compare a b) withScores

findIndices :: Int -> [Int]
findIndices n = map fst validIndices
  where
    boolArr = [testBit hand n | hand <- map fst sortedHands]
    arrWithIndices = zip [0..] boolArr
    validIndices = filter snd arrWithIndices


cardToIndices :: [Set Int]
cardToIndices = [fromDistinctAscList (findIndices x) | x <- [0..51]]

-- a list of length 52 where
-- lookupTables!!i is the indices
-- of sevenToBestFive that include card i.
lookupTables :: Array Int IntSet
lookupTables = array (0, 51) (zip [0..51] ([genLookups card | card <- [0..51]]))
  where
    -- can't refactor this at all because the combinations call will
    -- get cached and we'll run out of memory...
    genLookups card = fromList $ map snd $ filter (\x -> elem card (fst x)) (zip (combinations 7 [0..51]) [0..])


