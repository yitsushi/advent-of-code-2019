module Demo.Main
  ( solve
  )
where

import qualified Demo.Part1                    as Part1
import qualified Demo.Part2                    as Part2

solve :: Int -> String -> String
solve 2 input = Part2.solve input
solve _ input = Part1.solve input
