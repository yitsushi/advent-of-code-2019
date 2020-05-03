module Day11.Part1
  ( solve
  )
where

import qualified Data.Map.Strict               as Map
import           Day11.Lib
import           IntcodeMachine

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = show $ Map.size board
 where
  robot'                  = ((0, 0), North)
  board'                  = Map.singleton (fst robot') Black
  (robot, board, machine) = paint robot' board' $ boot $ loadComputer input [0]
