module Day15.Part1
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Day15.Lib
import Intcode

controller :: Drone -> Maybe Direction
controller drone
  | code == HitWall = error "We hit a wall"
  | code == Moved = error "Nothing happened"
  | code == Found = error "This is the OxygenSystem"
  | otherwise = Nothing
  where
    code = responseCode drone

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show $ mapAround $ executeDrone drone
  where
    tape = parse input
    drone = newDrone tape controller
