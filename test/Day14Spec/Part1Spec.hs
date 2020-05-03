module Day14Spec.Part1Spec where

import           Day14.Part1
import           Helper
import           Test.Hspec              hiding ( example )

example :: String -> String -> String -> SpecWith (Arg Expectation)
example title file expected = do
  content <- runIO $ loadFixture "day14" file
  it title $ solve content `shouldBe` expected

spec :: Spec
spec = do
  example "Example #1" "example1" "31"
  example "Example #2" "example2" "165"
  example "Example #3" "example3" "13312"
  example "Example #4" "example4" "180697"
  example "Example #5" "example5" "2210736"
