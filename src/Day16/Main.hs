module Day16.Main
  ( solve
  )
where

-- Day 16: Flawed Frequency Transmission

import qualified Day16.Part1                   as Part1
import qualified Day16.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
