module Day08.Part2
  ( solve
  ) where

import Data.Char
import Day08.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = unlines $ render image
  where
    bytes = (filter (>= 0) . stringToIntList) input
    image = parseImage (25, 6) bytes
