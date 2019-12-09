module IntcodeTests.ResolveValueSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "resolveValue" $ do
    it "Pointer" $ resolveValue [99, 88, 77] 0 (Pointer, 1) `shouldBe` 88
    it "Pointer (extra)" $
      resolveValue [99, 88, 77] 0 (Pointer, 100) `shouldBe` 0
    it "Immediate" $ resolveValue [99, 88, 77] 0 (Immediate, 1) `shouldBe` 1
    it "Relative" $ resolveValue [99, 88, 77] 1 (Relative, 1) `shouldBe` 77
    it "Relative (-)" $ resolveValue [99, 88, 77] 1 (Relative, -1) `shouldBe` 99
