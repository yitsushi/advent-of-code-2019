module Demo.Part1
  ( solve
  )
where

import           Demo.Lib

solve :: String -> String
solve = show . sum . map parseInt . words
