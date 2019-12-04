module Day04.Part1
  ( solve
  ) where

import Day04.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input =
  (show .
   length . filter validPassword . allPossibleCode . toBound . map read . words)
    input
