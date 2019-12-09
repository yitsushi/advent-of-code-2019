module Day05.Part1
  ( solve
  ) where

import Intcode

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show $ last $ getOutput $ execute $ Computer tape' [1] 0 [] 0
  where
    tape' = parse input
