module Day13.Part1
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Day13.Lib
import IntcodeMachine

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (show . length . Map.filter (== Block)) screen
  where
    tape = parse input
    game = newGame (loadComputer input []) (const 0)
    screen = (gameScreen . startGame) game
