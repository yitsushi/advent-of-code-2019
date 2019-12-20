module Day18Spec.Part1Spec where

import Day18.Part1
import Day18Spec.Helper
import Helper
import Test.Hspec hiding (example)

spec :: Spec
spec = do
  example solve "Example #1" "example1" "132"
  example solve "Example #2" "example2" "136"
  example solve "Example #3" "example3" "81"
