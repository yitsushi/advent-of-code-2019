module IntcodeTests.ParseSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "parse" $ do
    it "example #1" $ parse "1,0,0,0,99" `shouldBe` [1, 0, 0, 0, 99]
    it "example #2" $ parse "2,3,0,3,99" `shouldBe` [2, 3, 0, 3, 99]
    it "example #3" $ parse "2,4,4,5,99,0" `shouldBe` [2, 4, 4, 5, 99, 0]
    it "example #4" $
      parse "1,1,1,4,99,5,6,0,99" `shouldBe` [1, 1, 1, 4, 99, 5, 6, 0, 99]
