module Day07.Part1
  ( solve
  ) where

import Data.List
import IntcodeMachine

solve :: String -> String
solve input = (show . maximum . map mapper . permutations) [0 .. 4]
  where
    initial = cleanComputerWithOutput [0]
    mapper = head . output . foldl execute initial
    tape = parse input
    execute :: Computer -> Int -> Computer
    execute computer sequence =
      boot $ newComputer tape (sequence : output computer)
