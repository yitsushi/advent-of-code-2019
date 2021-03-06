module Day12.Main
  ( solve
  )
where

-- Day 12: The N-Body Problem

import qualified Day12.Part1                   as Part1
import qualified Day12.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
