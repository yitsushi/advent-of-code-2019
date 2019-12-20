module Day14Spec.Helper where

import Day14.Lib
import Day14.ResourceDatabase
import Helper
import Test.Hspec hiding (example)

loadDatabase :: String -> IO ResourceDatabase
loadDatabase file = do
  content <- loadFixture "day14" file
  let db =
        (foldl saveResource newResourceDatabase . map parseRecipe . lines)
          content
  pure db

example :: Solver -> String -> String -> String -> SpecWith (Arg Expectation)
example = solveExample "day14"
