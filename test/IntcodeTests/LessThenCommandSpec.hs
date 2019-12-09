module IntcodeTests.LessThenCommandSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "lessThenCommand" $ do
    it "writes 0 with pointer" $
      lessThanCommand
        (Computer [7, 5, 6, 0, 0, 3, 2] [] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [0, 5, 6, 0, 0, 3, 2] [] 4 [] 0
    it "writes 1 with pointer" $
      lessThanCommand
        (Computer [7, 5, 6, 0, 0, 2, 3] [] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [1, 5, 6, 0, 0, 2, 3] [] 4 [] 0
    it "writes 1 with immediate" $
      lessThanCommand
        (Computer [1107, 5, 6, 1, 0, 2, 3] [] 0 [] 0)
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1107, 1, 6, 1, 0, 2, 3] [] 4 [] 0
    it "writes 0 with immediate" $
      lessThanCommand
        (Computer [1107, 6, 5, 1, 0, 2, 3] [] 0 [] 0)
        [Immediate, Immediate, Pointer] `shouldBe`
      Computer [1107, 0, 5, 1, 0, 2, 3] [] 4 [] 0
