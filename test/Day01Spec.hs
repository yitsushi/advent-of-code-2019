module Day01Spec where

import Day01.Part1 as P1
import Day01.Part2 as P2
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "example #1" $ P1.solve "12" `shouldBe` "2"
    it "example #2" $ P1.solve "14" `shouldBe` "2"
    it "example #3" $ P1.solve "1969" `shouldBe` "654"
    it "example #4" $ P1.solve "100756" `shouldBe` "33583"
  describe "Part2" $ do
    it "example #1" $ P2.solve "14" `shouldBe` "2"
    it "example #2" $ P2.solve "1969" `shouldBe` "966"
    it "example #4" $ P2.solve "100756" `shouldBe` "50346"
