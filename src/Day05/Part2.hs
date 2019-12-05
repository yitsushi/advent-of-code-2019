module Day05.Part2
  ( solve
  ) where

import Intcode

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show $ getOutput $ execute $ Computer tape' [5] 0 []
  where
    tape' = parse input
