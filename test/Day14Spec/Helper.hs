module Day14Spec.Helper where

import           Helper

import           Day14.Lib
import           Day14.ResourceDatabase

loadDatabase :: String -> IO ResourceDatabase
loadDatabase file = do
  content <- loadFixture "day14" file
  let
    db =
      (foldl saveResource newResourceDatabase . map parseRecipe . lines) content
  pure db
