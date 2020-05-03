module Day03.Part1
  ( solve
  )
where

import qualified Data.Set
import           Day03.Lib
import           Lib

findCrossroads :: [Data.Set.Set Vector] -> Data.Set.Set Vector
findCrossroads (aPath : bPath : _) = Data.Set.intersection aPath bPath

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input =
  ( show
    . Data.Set.findMin
    . Data.Set.map manhattan
    . findCrossroads
    . map (Data.Set.fromList . allPointsOnPath . parseWire)
    . words
    )
    input
