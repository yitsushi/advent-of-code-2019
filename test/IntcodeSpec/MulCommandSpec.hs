module IntcodeSpec.MulCommandSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "mulCommand" $ do
    it "Two Pointers" $
      mulCommand
        (Computer [2, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [2, 9, 10, 1200, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 [] 0
    it "One Pointer and one Immediate" $
      mulCommand
        (Computer [2, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [] 0)
        [Immediate, Pointer, Pointer] `shouldBe`
      Computer [2, 9, 10, 360, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 [] 0
    it "Two Immediate" $
      mulCommand
        (Computer [2, 9, 10, 3, 2, 3] [] 0 [] 0)
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [2, 9, 10, 90, 2, 3] [] 4 [] 0
    it "RelativeBase values" $
      mulCommand
        (Computer [1, 2, 3, 4, 5, 6, 7, 8] [] 0 [] 3)
        [Relative, Relative, Pointer] `shouldBe`
      Computer [1, 2, 3, 4, 42, 6, 7, 8] [] 4 [] 3
    it "RelativeBase values in target" $
      mulCommand
        (Computer [1, 2, 3, 4, 5, 6, 7, 8] [] 0 [] 3)
        [Relative, Relative, Relative] `shouldBe`
      Computer [1, 2, 3, 4, 5, 6, 7, 42] [] 4 [] 3
    it "RelativeBase values in target out of reach" $
      mulCommand
        (Computer [1, 2, 3, 6, 5, 6, 7, 8] [] 0 [] 3)
        [Relative, Relative, Relative] `shouldBe`
      Computer [1, 2, 3, 6, 5, 6, 7, 8, 0, 42] [] 4 [] 3
