module Day24.Main
  ( solve
  )
where

-- Day 24: Planet of Discord

import qualified Day24.Part1                   as Part1
import qualified Day24.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
