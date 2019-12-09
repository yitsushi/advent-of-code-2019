module Day02.Part1
  ( solve
  ) where

import Intcode

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show $ flip valueInRegister 0 $ execute $ Computer tape [] 0 [] 0
  where
    tape' = parse input
    tape = head tape' : 12 : 2 : drop 3 tape'
