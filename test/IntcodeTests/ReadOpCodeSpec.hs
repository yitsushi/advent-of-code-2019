module IntcodeTests.ReadOpCodeSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "readOpCode" $ do
    it "1" $ readOpCode 1 `shouldBe` (1, [Pointer, Pointer, Pointer])
    it "2" $ readOpCode 2 `shouldBe` (2, [Pointer, Pointer, Pointer])
    it "102" $ readOpCode 102 `shouldBe` (2, [Immediate, Pointer, Pointer])
    it "205" $ readOpCode 205 `shouldBe` (5, [Relative, Pointer, Pointer])
    it "1002" $ readOpCode 1002 `shouldBe` (2, [Pointer, Immediate, Pointer])
    it "2005" $ readOpCode 2005 `shouldBe` (5, [Pointer, Relative, Pointer])
    it "10002" $ readOpCode 10002 `shouldBe` (2, [Pointer, Pointer, Immediate])
    it "20005" $ readOpCode 20005 `shouldBe` (5, [Pointer, Pointer, Relative])
    it "10102" $
      readOpCode 10102 `shouldBe` (2, [Immediate, Pointer, Immediate])
    it "20205" $ readOpCode 20205 `shouldBe` (5, [Relative, Pointer, Relative])
