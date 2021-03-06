module Day07.Main
  ( solve
  )
where

-- Day 7: Amplification Circuit

import qualified Day07.Part1                   as Part1
import qualified Day07.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
