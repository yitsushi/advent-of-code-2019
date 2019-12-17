module Day15.Part2
  ( solve
  ) where

import Data.Maybe
import qualified Data.WalkableMap as WM
import Day15.Lib
import Debug.Trace
import Intcode

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input
  | trace (unlines $ WM.draw (last area) tileChar) False = undefined
  | otherwise = show $ length area
  where
    tape = parse input
    drone = executeDrone $ newDrone tape controller
    area = takeWhile (not . null . WM.findValue Empty) (cycle (mapAround drone))
    cycle x = x : cycle next
      where
        next = spreadOxygen x
