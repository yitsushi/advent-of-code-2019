module Day14Spec.Part1Spec where

import Day14.Part1
import Day14Spec.Helper
import Test.Hspec hiding (example)

spec :: Spec
spec = do
  example solve "Example #1" "example1" "31"
  example solve "Example #2" "example2" "165"
  example solve "Example #3" "example3" "13312"
  example solve "Example #4" "example4" "180697"
  example solve "Example #5" "example5" "2210736"
