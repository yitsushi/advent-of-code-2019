module Day01.Main (solve) where

import qualified Day01.Part1 as Part1
import qualified Day01.Part2 as Part2

solve :: Int -> String -> IO ()
solve 2 input = Part2.solve input
solve _ input = Part1.solve input