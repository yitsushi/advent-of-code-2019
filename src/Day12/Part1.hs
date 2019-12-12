module Day12.Part1
  ( solve
  ) where

import Day12.Lib
import Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (show . sum . map totalEnergy . (!! 1000) . execute) moons
  where
    moons = (map (newMoon . (\x -> read x :: Vector)) . lines) input
