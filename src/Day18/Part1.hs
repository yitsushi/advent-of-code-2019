module Day18.Part1
  ( solve
  ) where

import qualified Data.Map.Strict as M
import           Day18.Lib
import           Debug.Trace

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input
  | trace (show $ collectedKeys gps) False = undefined
  | otherwise = show steps
  where
    (gps, steps) = planPedometer (newGPS input, 0)
