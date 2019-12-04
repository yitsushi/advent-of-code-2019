module Day04Spec where

import Day04.Lib as Lib
import Test.Hspec

spec :: Spec
spec = do
  describe "validPassword" $ do
    it "111111 is Valid" $ Lib.validPassword 111111 `shouldBe` True
    it "223450 is Not Valid" $ Lib.validPassword 223450 `shouldBe` False
    it "123789 is Not Valid" $ Lib.validPassword 123789 `shouldBe` False
    it "123789 is Not Valid" $ Lib.validPassword 123789 `shouldBe` False
  describe "hasDouble" $ do
    it "112233" $ Lib.hasDouble "112233" `shouldBe` True
    it "123444" $ Lib.hasDouble "123444" `shouldBe` False
    it "111122" $ Lib.hasDouble "111122" `shouldBe` True
