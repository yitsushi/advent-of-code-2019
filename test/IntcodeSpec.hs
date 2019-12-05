module IntcodeSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec = do
  describe "parse" $ do
    it "example #1" $ parse "1,0,0,0,99" `shouldBe` [1, 0, 0, 0, 99]
    it "example #2" $ parse "2,3,0,3,99" `shouldBe` [2, 3, 0, 3, 99]
    it "example #3" $ parse "2,4,4,5,99,0" `shouldBe` [2, 4, 4, 5, 99, 0]
    it "example #4" $
      parse "1,1,1,4,99,5,6,0,99" `shouldBe` [1, 1, 1, 4, 99, 5, 6, 0, 99]
  describe "readOpCode" $ do
    it "1" $ readOpCode 1 `shouldBe` (1, [Pointer, Pointer, Pointer])
    it "2" $ readOpCode 2 `shouldBe` (2, [Pointer, Pointer, Pointer])
    it "102" $ readOpCode 102 `shouldBe` (2, [Immediate, Pointer, Pointer])
    it "1002" $ readOpCode 1002 `shouldBe` (2, [Pointer, Immediate, Pointer])
    it "10002" $ readOpCode 10002 `shouldBe` (2, [Pointer, Pointer, Immediate])
    it "10102" $
      readOpCode 10102 `shouldBe` (2, [Immediate, Pointer, Immediate])
  describe "resolveValue" $ do
    it "Pointer" $ resolveValue [99, 88, 77] (Pointer, 1) `shouldBe` 88
    it "Immediate" $ resolveValue [99, 88, 77] (Immediate, 1) `shouldBe` 1
  describe "addCommand" $ do
    it "Two Pointers" $
      addCommand
        (Computer [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [])
        [Pointer, Pointer, Immediate] `shouldBe`
      Computer [1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 []
    it "One Pointer and one Immediate" $
      addCommand
        (Computer [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [])
        [Immediate, Pointer, Immediate] `shouldBe`
      Computer [1, 9, 10, 49, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 []
    it "Two Immediate" $
      addCommand
        (Computer [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [])
        [Immediate, Immediate, Immediate] `shouldBe`
      Computer [1, 9, 10, 19, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 []
  describe "addCommand" $ do
    it "Two Pointers" $
      mulCommand
        (Computer [2, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [])
        [Pointer, Pointer, Immediate] `shouldBe`
      Computer [2, 9, 10, 1200, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 []
    it "One Pointer and one Immediate" $
      mulCommand
        (Computer [2, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [])
        [Immediate, Pointer, Immediate] `shouldBe`
      Computer [2, 9, 10, 360, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 []
    it "Two Immediate" $
      mulCommand
        (Computer [2, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [])
        [Immediate, Immediate, Immediate] `shouldBe`
      Computer [2, 9, 10, 90, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 []
