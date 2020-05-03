module Day09.Part1
  ( solve
  )
where

import           IntcodeMachine

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = (show . head . output . boot) (loadComputer input [1])
