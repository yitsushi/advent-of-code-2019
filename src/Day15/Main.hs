module Day15.Main
  ( solve
  )
where

-- Day 15: Oxygen System

import qualified Day15.Part1                   as Part1
import qualified Day15.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
