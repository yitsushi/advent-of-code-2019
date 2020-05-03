module Day01Spec.Part2Spec where

import           Day01.Part2
import           Test.Hspec

spec :: Spec
spec = do
  it "example #1" $ solve "14" `shouldBe` "2"
  it "example #2" $ solve "1969" `shouldBe` "966"
  it "example #4" $ solve "100756" `shouldBe` "50346"
