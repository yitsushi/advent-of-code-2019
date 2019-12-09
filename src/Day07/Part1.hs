module Day07.Part1
  ( solve
  ) where

import Data.List
import Intcode

solve :: String -> String
solve input =
  (show .
   maximum .
   map (head . getOutput . foldl execute' (Computer [] [] 0 [0] 0)) .
   permutations)
    [0 .. 4]
  where
    tape = parse input
    execute' :: Computer -> Int -> Computer
    execute' (Computer _ _ _ output _) sequence =
      execute $ Computer tape (sequence : output) 0 [] 0
