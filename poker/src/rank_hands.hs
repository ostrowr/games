-- must be compiled with -fno-full-laziness

import           Poker
import Data.IntSet (size, intersection)
import Data.Array.IArray


main :: IO ()
main = do
  let l = lookupTables
  print $ size $ l!1
  print $ size $ intersection (l!1) (l!2)
