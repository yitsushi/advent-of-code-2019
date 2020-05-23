module Day08.Main
  ( solve
  )
where

-- Day 8: Space Image Format

import qualified Day08.Part1                   as Part1
import qualified Day08.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
