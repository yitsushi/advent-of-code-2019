module Day13.Part1
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Day13.Lib
import Intcode

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (show . length . Map.filter (== Block)) screen
  where
    tape = parse input
    game = newGame (Computer tape [] 0 [] 0) (const 0)
    screen = (gameScreen . startGame) game
