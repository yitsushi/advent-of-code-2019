module Day11.Main
  ( solve
  )
where

-- Day 11: Space Police

import qualified Day11.Part1                   as Part1
import qualified Day11.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
