module Day01.Main
  ( solve
  )
where

-- Day 1: The Tyranny of the Rocket Equation

import qualified Day01.Part1                   as Part1
import qualified Day01.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
