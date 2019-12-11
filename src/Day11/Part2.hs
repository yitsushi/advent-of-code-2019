module Day11.Part2
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Day11.Lib
import Intcode

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = unlines $ map (map colorToChar) $ render board
  where
    tape = parse input
    robot' = ((0, 0), North)
    board' = Map.singleton (fst robot') White
    (robot, board, machine) =
      paint robot' board' (execute (Computer tape [1] 0 [] 0))
    colorToChar Black = ' '
    colorToChar White = '#'
