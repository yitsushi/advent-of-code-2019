module Day03.Main
  ( solve
  )
where

-- Day 3: Crossed Wires

import qualified Day03.Part1                   as Part1
import qualified Day03.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
