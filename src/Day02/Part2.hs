module Day02.Part2
  ( solve
  ) where

import Control.Monad
import Day02.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (join . map show . take 2 . tail) final
  where
    tape' = parse input
    possibilities =
      [ head tape' : noun : verb : drop 3 tape'
      | noun <- [0 .. 99]
      , verb <- [0 .. 99]
      ]
    process' tape = process 0 (getOps 0 tape) tape
    final = head $ dropWhile (validate 19690720) $ map process' possibilities
      where
        validate value (x:xs) = value /= x
