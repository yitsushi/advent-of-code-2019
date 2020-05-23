module Day05.Main
  ( solve
  )
where

-- Day 5: Sunny with a Chance of Asteroids

import qualified Day05.Part1                   as Part1
import qualified Day05.Part2                   as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
