module Day05.Part2
  ( solve
  )
where

import           IntcodeMachine          hiding ( input )

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = (show . last . output . boot) (loadComputer input [5])
