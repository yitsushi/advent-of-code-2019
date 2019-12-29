module Day02.Part2
  ( solve
  ) where

import Control.Monad
import IntcodeMachine

solve :: String -> String
solve input = (join . map show . getNounVerb) final
  where
    tape = parse input
    possibilities =
      [ newComputer (head tape : noun : verb : drop 3 tape) []
      | noun <- [0 .. 99]
      , verb <- [0 .. 99]
      ]
    final = (head . dropWhile (validate 19690720) . map boot) possibilities
      where
        validate value box = value /= head (memory box)
    getNounVerb = take 2 . drop 1 . memory
