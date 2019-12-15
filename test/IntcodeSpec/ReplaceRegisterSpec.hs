module IntcodeSpec.ReplaceRegisterSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "replaceRegister" $ do
    it "at the beginning" $ replaceRegister [1, 2, 3] 0 99 `shouldBe` [99, 2, 3]
    it "at the end" $ replaceRegister [1, 2, 3] 2 99 `shouldBe` [1, 2, 99]
    it "in the middle" $ replaceRegister [1, 2, 3] 1 99 `shouldBe` [1, 99, 3]
    it "out of reach" $
      replaceRegister [1, 2, 3] 5 99 `shouldBe` [1, 2, 3, 0, 0, 99]
