module Day14.Part1
  ( solve
  )
where

import           Day14.Lib
import           Day14.Pocket
import           Day14.ResourceDatabase

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = show (oreForFuel database 1)
 where
  database =
    (foldl saveResource newResourceDatabase . map parseRecipe . lines) input
