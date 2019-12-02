module Day02Spec where

import Day02.Lib as Lib
import Test.Hspec

processWrapper :: [Int] -> [Int]
processWrapper tape = Lib.process 0 (take 4 tape) tape

simpleTape = [1, 1, 1, 4, 2, 5, 6, 0, 99]

spec :: Spec
spec = do
  describe "parse" $ do
    it "example #1" $ Lib.parse "1,0,0,0,99" `shouldBe` [1, 0, 0, 0, 99]
    it "example #2" $ Lib.parse "2,3,0,3,99" `shouldBe` [2, 3, 0, 3, 99]
    it "example #3" $ Lib.parse "2,4,4,5,99,0" `shouldBe` [2, 4, 4, 5, 99, 0]
    it "example #4" $
      Lib.parse "1,1,1,4,99,5,6,0,99" `shouldBe` [1, 1, 1, 4, 99, 5, 6, 0, 99]
  describe "getOps" $ do
    it "example #1" $ Lib.getOps 0 simpleTape `shouldBe` [1, 1, 1, 4]
    it "example #2" $ Lib.getOps 4 simpleTape `shouldBe` [2, 5, 6, 0]
  describe "process" $ do
    it "example #1" $
      processWrapper [1, 0, 0, 0, 99] `shouldBe` [2, 0, 0, 0, 99]
    it "example #2" $
      processWrapper [2, 3, 0, 3, 99] `shouldBe` [2, 3, 0, 6, 99]
    it "example #3" $
      processWrapper [2, 4, 4, 5, 99, 0] `shouldBe` [2, 4, 4, 5, 99, 9801]
    it "example #4" $
      processWrapper [1, 1, 1, 4, 99, 5, 6, 0, 99] `shouldBe`
      [30, 1, 1, 4, 2, 5, 6, 0, 99]
