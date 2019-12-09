module IntcodeTests.JumpIfFalseCommandSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "jumpIfFalseCommand" $ do
    it "jump pointer" $
      jumpIfFalseCommand
        (Computer [6, 2, 10, 3, 2, 3] [1] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [6, 2, 10, 3, 2, 3] [1] 3 [] 0
    it "no jump pointer" $
      jumpIfFalseCommand
        (Computer [6, 2, 0, 3, 2, 3] [1] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [6, 2, 0, 3, 2, 3] [1] 6 [] 0
    it "jump immediate" $
      jumpIfFalseCommand
        (Computer [1106, 2, 10, 3, 2, 3] [1] 0 [] 0)
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1106, 2, 10, 3, 2, 3] [1] 3 [] 0
    it "no jump immediate" $
      jumpIfFalseCommand
        (Computer [1106, 0, 0, 3, 2, 3] [1] 0 [] 0)
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1106, 0, 0, 3, 2, 3] [1] 0 [] 0
