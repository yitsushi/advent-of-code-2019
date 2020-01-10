module Day17.Part1
  ( solve
  ) where

import Day17.Lib
import IntcodeMachine

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show $ sum $ map (uncurry (*)) intersections
  where
    tape = parse input
    machine = newMachine tape
    machine' = executeMachine machine
    intersections = findIntersections $ machineScreen machine'
