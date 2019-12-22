module Day22Spec.LibSpec where

import Day22.Lib
import Test.Hspec

spec :: Spec
spec =
  describe "shuffle" $ do
    describe "deal into new stack" $
      it "deal into new stack" $ shuffle [0 .. 9] Reverse `shouldBe` [9,8 .. 0]
    describe "cut N" $ do
      it "cut 3" $
        shuffle [0 .. 9] (Cut 3) `shouldBe` [3, 4, 5, 6, 7, 8, 9, 0, 1, 2]
      it "cut -4" $
        shuffle [0 .. 9] (Cut (-4)) `shouldBe` [6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
    describe "deal with increment N" $
      it "deal with increment 3" $
      shuffle [0 .. 9] (Increment 3) `shouldBe` [0, 7, 4, 1, 8, 5, 2, 9, 6, 3]
