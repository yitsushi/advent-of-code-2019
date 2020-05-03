module Day04.Part2
  ( solve
  )
where

import           Day04.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input =
  ( show
    . length
    . filter validate
    . allPossibleCode
    . toBound
    . map read
    . words
    )
    input
  where validate code = validPassword code && hasDouble (show code)
