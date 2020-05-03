module Day16.Part1
  ( solve
  )
where

import           Day16.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = process' input
 where
  process' =
    concatMap show . take 8 . last . take 101 . iterate process . parse
