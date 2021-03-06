module Day17.Main
  ( solve
  )
where

-- Day 17: Set and Forget

import qualified Day17.Part1                   as Part1
import qualified Day17.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
