module Day22.Part1
  ( solve
  )
where

import           Data.List
import           Day22.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = show $ elemIndex 2019 $ foldl shuffle deck instructions
 where
  instructions = (map parseLine . lines) input
  deck         = [0 .. 10006]
