module Day02.Part1
  ( solve
  ) where

import Day02.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show $ head $ process 0 (getOps 0 tape) tape
  where
    tape' = parse input
    tape = head tape' : 12 : 2 : drop 3 tape'
