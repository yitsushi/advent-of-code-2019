module IntcodeSpec.AdjustBaseCommandSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "adjustBaseCommand" $ do
    it "Pointer" $
      adjustBaseCommand
        (Computer [1, 2, 3, 4, 5, 6] [1] 0 [] 10)
        [Pointer, Pointer, Pointer] `shouldBe`
      Computer [1, 2, 3, 4, 5, 6] [1] 2 [] 13
    it "Immediate" $
      adjustBaseCommand
        (Computer [101, 2, 3, 4, 5, 6] [1] 0 [] 10)
        [Immediate, Pointer, Pointer] `shouldBe`
      Computer [101, 2, 3, 4, 5, 6] [1] 2 [] 12
    it "Relative" $
      adjustBaseCommand
        (Computer [201, 2, 3, 4, 5, 6] [1] 0 [] 10)
        [Relative, Pointer, Pointer] `shouldBe`
      Computer [201, 2, 3, 4, 5, 6] [1] 2 [] 10
