module Demo.Part1
  ( solve
  ) where

import Demo.Lib

solve :: String -> IO ()
solve = print . sum . map parseInt . words
