module Main where

import           Poker
import Data.IntSet (size, intersection, showTree)
import Data.Array.IArray
import Codec.Archive.Zip

main :: IO ()
main = do
  let l = lookupTables
  let l1 = l!1
  let l2 = l!2
  let l3 = l!3
  let l4 = l!4
  let l5 = l!5
  let l6 = l!6
  let l12 = intersection l1 l2
  let l123 = intersection l12 l3
  let l1234 = intersection l123 l4
  let l12345 = intersection l1234 l5
  let l123456 = intersection l12345 l6
  print $ showTree (l123456)
  -- print $ size $ intersection (l!1) (l!2)
