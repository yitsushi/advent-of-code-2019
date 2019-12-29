module Day13.Part2
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Day13.Lib
import IntcodeMachine

controller :: Game -> Int
controller game
  | Map.size (gameScreen game) < 2 = 0
  | xBall > xPlayer = 1
  | xBall < xPlayer = -1
  | otherwise = 0
  where
    (xBall, _) = getBallPosition game
    (xPlayer, _) = getPlayerPosition game

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (show . gameScore . startGame) game
  where
    tape = parse input
    game = newGame (newComputer (2 : tail tape) []) controller
