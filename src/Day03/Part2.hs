module Day03.Part2
  ( solve
  ) where

import qualified Data.Map
import           Day03.Lib
import           Lib

findCrossroads :: [Data.Map.Map Vector Int] -> Data.Map.Map Vector Int
findCrossroads (aPath:bPath:_) = Data.Map.intersectionWith (+) aPath bPath

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input =
  (show .
   minimum .
   Data.Map.elems .
   findCrossroads .
   map
     (Data.Map.fromListWith (+) . flip zip [1 ..] . allPointsOnPath . parseWire) .
   words)
    input
