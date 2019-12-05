module Day02.Part2
  ( solve
  ) where

import Control.Monad
import Intcode

solve input = (join . map show . getNounVerb) final
  where
    tape' = parse input
    possibilities =
      [ Computer (head tape' : noun : verb : drop 3 tape') [] 0 []
      | noun <- [0 .. 99]
      , verb <- [0 .. 99]
      ]
    final = (head . dropWhile (validate 19690720) . map execute) possibilities
      where
        validate value box = value /= valueInRegister box 0
