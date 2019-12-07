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
  describe "inputCommand" $ do
    it "example #1" $
      inputCommand
        (Computer [4, 5, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [4, 5, 10, 3, 2, 1, 11, 0, 99, 30, 40, 50] [] 2 []
    it "example #1" $
      inputCommand
        (Computer [4, 3, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [4, 3, 10, 1, 2, 3, 11, 0, 99, 30, 40, 50] [] 2 []
    it "multiple input value" $
      inputCommand
        (Computer [4, 3, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1, 2] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [4, 3, 10, 1, 2, 3, 11, 0, 99, 30, 40, 50] [2] 2 []
  describe "outputCommand" $ do
    it "example #1" $
      outputCommand
        (Computer [3, 5, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [3, 5, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 2 [3]
    it "example #1" $
      outputCommand
        (Computer [4, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [4, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 2 [10]
  describe "jumpIfTrueCommand" $ do
    it "jump pointer" $
      jumpIfTrueCommand
        (Computer [5, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [5, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 40 []
    it "no jump pointer" $
      jumpIfTrueCommand
        (Computer [5, 2, 0, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [5, 2, 0, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 3 []
    it "jump immediate" $
      jumpIfTrueCommand
        (Computer [1105, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1105, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 10 []
    it "no jump immediate" $
      jumpIfTrueCommand
        (Computer [1105, 0, 4, 3, 0, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1105, 0, 4, 3, 0, 3, 11, 0, 99, 30, 40, 50] [1] 3 []
  describe "jumpIfFalseCommand" $ do
    it "jump pointer" $
      jumpIfFalseCommand
        (Computer [6, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [6, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 3 []
    it "no jump pointer" $
      jumpIfFalseCommand
        (Computer [6, 2, 0, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [6, 2, 0, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 6 []
    it "jump immediate" $
      jumpIfFalseCommand
        (Computer [1106, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1106, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 3 []
    it "no jump immediate" $
      jumpIfFalseCommand
        (Computer [1106, 0, 0, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [])
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1106, 0, 0, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 []
  describe "lessThenCommand" $ do
    it "writes 0 with pointer" $
      lessThanCommand
        (Computer [7, 5, 6, 0, 0, 3, 2] [] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [0, 5, 6, 0, 0, 3, 2] [] 4 []
    it "writes 1 with pointer" $
      lessThanCommand
        (Computer [7, 5, 6, 0, 0, 2, 3] [] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [1, 5, 6, 0, 0, 2, 3] [] 4 []
    it "writes 1 with immediate" $
      lessThanCommand
        (Computer [1107, 5, 6, 1, 0, 2, 3] [] 0 [])
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1107, 1, 6, 1, 0, 2, 3] [] 4 []
    it "writes 0 with immediate" $
      lessThanCommand
        (Computer [1107, 6, 5, 1, 0, 2, 3] [] 0 [])
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1107, 0, 5, 1, 0, 2, 3] [] 4 []
  describe "equalsCommand" $ do
    it "writes 0 with pointer" $
      equalsCommand
        (Computer [7, 5, 6, 0, 0, 2, 3] [] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [0, 5, 6, 0, 0, 2, 3] [] 4 []
    it "writes 1 with pointer" $
      equalsCommand
        (Computer [7, 5, 6, 0, 0, 3, 3] [] 0 [])
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [1, 5, 6, 0, 0, 3, 3] [] 4 []
    it "writes 1 with immediate" $
      equalsCommand
        (Computer [1107, 6, 6, 1, 0, 2, 3] [] 0 [])
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1107, 1, 6, 1, 0, 2, 3] [] 4 []
    it "writes 0 with immediate" $
      equalsCommand
        (Computer [1107, 5, 6, 1, 0, 2, 3] [] 0 [])
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1107, 0, 6, 1, 0, 2, 3] [] 4 []
