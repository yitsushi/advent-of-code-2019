module IntcodeSpec.AddCommandSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "addCommand" $ do
    it "Two Pointers" $
      addCommand
        (Computer [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 [] 0
    it "One Pointer and one Immediate" $
      addCommand
        (Computer [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [] 0)
        [Immediate, Pointer, Pointer] `shouldBe`
      Computer [1, 9, 10, 49, 2, 3, 11, 0, 99, 30, 40, 50] [] 4 [] 0
    it "Two Immediate" $
      addCommand
        (Computer [1, 9, 10, 3, 2, 3] [] 0 [] 0)
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1, 9, 10, 19, 2, 3] [] 4 [] 0
    it "RelativeBase values" $
      addCommand
        (Computer [1, 2, 3, 4, 5, 6, 7, 8] [] 0 [] 3)
        [Relative, Relative, Pointer] `shouldBe`
      Computer [1, 2, 3, 4, 13, 6, 7, 8] [] 4 [] 3
    it "RelativeBase values in target" $
      addCommand
        (Computer [1, 2, 3, 4, 5, 6, 7, 8] [] 0 [] 3)
        [Relative, Relative, Relative] `shouldBe`
      Computer [1, 2, 3, 4, 5, 6, 7, 13] [] 4 [] 3
    it "RelativeBase values in target out of reach" $
      addCommand
        (Computer [1, 2, 3, 6, 5, 6, 7, 8] [] 0 [] 3)
        [Relative, Relative, Relative] `shouldBe`
      Computer [1, 2, 3, 6, 5, 6, 7, 8, 0, 13] [] 4 [] 3
