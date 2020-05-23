module Day04.Main
  ( solve
  )
where

-- Day 4: Secure Container

import qualified Day04.Part1                   as Part1
import qualified Day04.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
