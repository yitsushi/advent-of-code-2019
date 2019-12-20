module Day18.Part1
  ( solve
  ) where

import Day18.Lib
import Debug.Trace

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show steps
  where
    cave' = parseCave input
    start = entrance cave'
    (gps, steps) =
      planPedometer
        (GPS {cave = cave', collectedKeys = [], position = start}, 0)
