module IntcodeSpec.JumpIfTrueCommandSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "jumpIfTrueCommand" $ do
    it "jump pointer" $
      jumpIfTrueCommand
        (Computer [5, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [5, 2, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [1] 40 [] 0
    it "no jump pointer" $
      jumpIfTrueCommand
        (Computer [5, 2, 0, 3, 2, 3] [1] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [5, 2, 0, 3, 2, 3] [1] 3 [] 0
    it "jump immediate" $
      jumpIfTrueCommand
        (Computer [1105, 2, 10, 3, 2, 3] [1] 0 [] 0)
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1105, 2, 10, 3, 2, 3] [1] 10 [] 0
    it "no jump immediate" $
      jumpIfTrueCommand
        (Computer [1105, 0, 4, 3, 0, 3] [1] 0 [] 0)
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1105, 0, 4, 3, 0, 3] [1] 3 [] 0
