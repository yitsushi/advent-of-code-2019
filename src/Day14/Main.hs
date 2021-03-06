module Day14.Main
  ( solve
  )
where

-- Day 14: Space Stoichiometry

import qualified Day14.Part1                   as Part1
import qualified Day14.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
