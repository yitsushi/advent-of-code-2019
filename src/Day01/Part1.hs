module Day01.Part1
  ( solve
  ) where

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (show . sum . map (fuel . read) . words) input
