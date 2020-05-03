module Day02.Part2
  ( solve
  )
where

import           Control.Monad
import           IntcodeMachine          hiding ( input )

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
    where validate value box = value /= readMemory 0 box
  getNounVerb computer = map (`readMemory` computer) [1, 2]
