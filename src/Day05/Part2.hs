module Day05.Part2
  ( solve
  ) where

import IntcodeMachine

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (show . last . output . boot) (loadComputer input [5])
