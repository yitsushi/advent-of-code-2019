module LibSpec where

import Lib
import Test.Hspec

spec :: Spec
spec = do
  describe "splitOn" $ do
    it "simple" $ splitOn ',' "1,2,3,4" `shouldBe` ["1", "2", "3", "4"]
    it "nothing to split" $ splitOn ',' "123" `shouldBe` ["123"]
    it "starts with separator" $ splitOn ',' ",1,2" `shouldBe` ["", "1", "2"]
    it "ends with separator" $ splitOn ',' "1,2," `shouldBe` ["1", "2", ""]
  describe "lpad" $ do
    it "simple" $ lpad '0' 5 "5" `shouldBe` "00005"
    it "equal to desired length" $ lpad '0' 3 "115" `shouldBe` "115"
    it "longer then desired length" $ lpad '0' 3 "1115" `shouldBe` "1115"
  describe "manhattan" $ do
    describe "IntVector2D" $ do
      it "( 0,  0)" $ manhattan (IntVector2D (0, 0)) `shouldBe` 0
      it "( 2,  2)" $ manhattan (IntVector2D (2, 2)) `shouldBe` 4
      it "(-2, -2)" $ manhattan (IntVector2D (-2, -2)) `shouldBe` 4
      it "(-2,  2)" $ manhattan (IntVector2D (-2, 2)) `shouldBe` 4
      it "( 2, -2)" $ manhattan (IntVector2D (2, -2)) `shouldBe` 4
    describe "IntVector3D" $ do
      it "( 0,  0,  0)" $ manhattan (IntVector3D (0, 0, 0)) `shouldBe` 0
      it "( 1,  1,  1)" $ manhattan (IntVector3D (1, 1, 1)) `shouldBe` 3
      it "(-1, -1, -1)" $ manhattan (IntVector3D (-1, -1, -1)) `shouldBe` 3
      it "( 1, -1, -1)" $ manhattan (IntVector3D (1, -1, -1)) `shouldBe` 3
      it "(-1,  1, -1)" $ manhattan (IntVector3D (-1, 1, -1)) `shouldBe` 3
      it "(-1, -1,  1)" $ manhattan (IntVector3D (-1, -1, 1)) `shouldBe` 3
