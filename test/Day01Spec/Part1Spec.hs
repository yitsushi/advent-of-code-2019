module Day01Spec.Part1Spec where

import           Day01.Part1
import           Test.Hspec

spec :: Spec
spec = do
  it "example #1" $ solve "12" `shouldBe` "2"
  it "example #2" $ solve "14" `shouldBe` "2"
  it "example #3" $ solve "1969" `shouldBe` "654"
  it "example #4" $ solve "100756" `shouldBe` "33583"
