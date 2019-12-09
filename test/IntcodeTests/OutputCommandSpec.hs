module IntcodeTests.OutputCommandSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "outputCommand" $ do
    describe "Pointer" $ do
      it "example #1" $
        outputCommand
          (Computer [3, 4, 10, 3, 2, 3] [1] 0 [] 0)
          [Pointer, Pointer, Pointer] `shouldBe`
        Computer [3, 4, 10, 3, 2, 3] [1] 2 [2] 0
      it "example #2" $
        outputCommand
          (Computer [4, 2, 10, 3, 2, 3] [1] 0 [] 0)
          [Pointer, Pointer, Pointer] `shouldBe`
        Computer [4, 2, 10, 3, 2, 3] [1] 2 [10] 0
      it "out of reach" $
        outputCommand
          (Computer [4, 100, 10, 3, 2, 3] [1] 0 [] 0)
          [Pointer, Pointer, Pointer] `shouldBe`
        Computer [4, 100, 10, 3, 2, 3] [1] 2 [0] 0
    describe "Relative" $ do
      it "example #1" $
        outputCommand
          (Computer [3, 4, 10, 3, 2, 3] [1] 0 [] 1)
          [Relative, Pointer, Pointer] `shouldBe`
        Computer [3, 4, 10, 3, 2, 3] [1] 2 [3] 1
      it "example #2" $
        outputCommand
          (Computer [4, 2, 10, 3, 2, 3] [1] 0 [] 1)
          [Relative, Pointer, Pointer] `shouldBe`
        Computer [4, 2, 10, 3, 2, 3] [1] 2 [3] 1
      it "out of reach" $
        outputCommand
          (Computer [4, 100, 10, 3, 2, 3] [1] 0 [] 1)
          [Relative, Pointer, Pointer] `shouldBe`
        Computer [4, 100, 10, 3, 2, 3] [1] 2 [0] 1
