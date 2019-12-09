module IntcodeTests.InputCommandSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "inputCommand" $ do
    it "example #1" $
      inputCommand
        (Computer [4, 5, 10, 3, 2, 3] [1] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [4, 5, 10, 3, 2, 1] [] 2 [] 0
    it "example #1" $
      inputCommand
        (Computer [4, 3, 10, 3, 2, 3] [1] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [4, 3, 10, 1, 2, 3] [] 2 [] 0
    it "multiple input value" $
      inputCommand
        (Computer [4, 3, 10, 3, 2, 3] [1, 2] 0 [] 0)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [4, 3, 10, 1, 2, 3] [2] 2 [] 0
