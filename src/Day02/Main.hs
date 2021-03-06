module Day02.Main
  ( solve
  )
where

-- Day 2: 1202 Program Alarm

import qualified Day02.Part1                   as Part1
import qualified Day02.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
