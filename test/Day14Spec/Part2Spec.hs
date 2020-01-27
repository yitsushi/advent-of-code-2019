module Day14Spec.Part2Spec where

import Day14.Part2
import Day14Spec.Helper
import Test.Hspec hiding (example)

spec :: Spec
spec = do
  example solve "Example #3" "example3" "82892753"
  example solve "Example #4" "example4" "5586022"
  example solve "Example #5" "example5" "460664"
