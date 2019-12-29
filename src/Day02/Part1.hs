module Day02.Part1
  ( solve
  ) where

import IntcodeMachine

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (show . head . memory . boot) computer
  where
    tape = parse input
    computer = newComputer (head tape : 12 : 2 : drop 3 tape) []
