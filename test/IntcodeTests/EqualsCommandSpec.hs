module IntcodeTests.EqualsCommandSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "equalsCommand" $ do
    describe "with pointers" $ do
      it "writes 0" $
        equalsCommand
          (Computer [7, 5, 6, 0, 0, 2, 3] [] 0 [] 0)
          [Pointer, Pointer, Pointer] `shouldBe`
        Computer [0, 5, 6, 0, 0, 2, 3] [] 4 [] 0
      it "writes 1" $
        equalsCommand
          (Computer [7, 5, 6, 0, 0, 3, 3] [] 0 [] 0)
          [Pointer, Pointer, Pointer] `shouldBe`
        Computer [1, 5, 6, 0, 0, 3, 3] [] 4 [] 0
    describe "with immediate" $ do
      it "writes 1" $
        equalsCommand
          (Computer [1107, 6, 6, 1, 0, 2, 3] [] 0 [] 0)
          [Immediate, Immediate, Pointer] `shouldBe`
        Computer [1107, 1, 6, 1, 0, 2, 3] [] 4 [] 0
      it "writes 0" $
        equalsCommand
          (Computer [1107, 5, 6, 1, 0, 2, 3] [] 0 [] 0)
          [Immediate, Immediate, Pointer] `shouldBe`
        Computer [1107, 0, 6, 1, 0, 2, 3] [] 4 [] 0
    describe "with relative source" $ do
      it "writes 1" $
        equalsCommand
          (Computer [1, 2, 2, 4, 5, 6, 7, 8] [] 0 [] 3)
          [Relative, Relative, Pointer] `shouldBe`
        Computer [1, 2, 2, 4, 1, 6, 7, 8] [] 4 [] 3
      it "writes 0" $
        equalsCommand
          (Computer [1, 2, 3, 4, 5, 6, 7, 8] [] 0 [] 3)
          [Relative, Relative, Pointer] `shouldBe`
        Computer [1, 2, 3, 4, 0, 6, 7, 8] [] 4 [] 3
    describe "with relative destination" $ do
      it "writes 1" $
        equalsCommand
          (Computer [1, 2, 2, 4, 5, 6, 7, 8] [] 0 [] 3)
          [Relative, Relative, Relative] `shouldBe`
        Computer [1, 2, 2, 4, 5, 6, 7, 1] [] 4 [] 3
      it "writes 0" $
        equalsCommand
          (Computer [1, 2, 3, 4, 5, 6, 7, 8] [] 0 [] 3)
          [Relative, Relative, Relative] `shouldBe`
        Computer [1, 2, 3, 4, 5, 6, 7, 0] [] 4 [] 3
