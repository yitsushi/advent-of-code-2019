module Day10.Part1
  ( solve
  ) where

import Data.Foldable
import Day10.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input =
  (show . snd . maximumBy ordering . calculateVisibility . parse . words) input
  where
    ordering :: MapPoint MapValue -> MapPoint MapValue -> Ordering
    ordering (_, connections1) (_, connections2)
      | connections1 < connections2 = LT
      | otherwise = GT
