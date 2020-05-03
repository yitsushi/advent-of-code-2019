module Day14Spec.Part2Spec where

import           Day14.Part2
import           Helper
import           Test.Hspec              hiding ( example )

example :: String -> String -> String -> SpecWith (Arg Expectation)
example title file expected = do
  content <- runIO $ loadFixture "day14" file
  it title $ solve content `shouldBe` expected

spec :: Spec
spec = do
  example "Example #3" "example3" "82892753"
  example "Example #4" "example4" "5586022"
  example "Example #5" "example5" "460664"
