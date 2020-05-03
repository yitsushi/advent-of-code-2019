module IntcodeMachineSpec.ExecuteSpec where

import           IntcodeMachine
import           Test.Hspec

spec :: Spec
spec = do
  describe "Day02 Examples" $ do
    it "example #1" $ do
      let final = boot $ loadComputer "1,9,10,3,2,3,11,0,99,30,40,50" []
      readMemory 0 final `shouldBe` 3500
      output final `shouldBe` []
      status final `shouldBe` Term
    it "example #2" $ do
      let final = boot $ loadComputer "1,0,0,0,99" []
      readMemory 0 final `shouldBe` 2
      output final `shouldBe` []
      status final `shouldBe` Term
    it "example #3" $ do
      let final = boot $ loadComputer "2,3,0,3,99" []
      readMemory 3 final `shouldBe` 6
      output final `shouldBe` []
      status final `shouldBe` Term
    it "example #4" $ do
      let final = boot $ loadComputer "2,4,4,5,99,0" []
      readMemory 5 final `shouldBe` 9801
      output final `shouldBe` []
      status final `shouldBe` Term
    it "example #4" $ do
      let final = boot $ loadComputer "1,1,1,4,99,5,6,0,99" []
      readMemory 0 final `shouldBe` 30
      output final `shouldBe` []
      status final `shouldBe` Term
  describe "Day05 Examples" $ do
    let
      tape
        = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    it "input < 8" $ do
      let final = boot $ loadComputer tape [7]
      last (output final) `shouldBe` 999
    it "input == 8" $ do
      let final = boot $ loadComputer tape [8]
      last (output final) `shouldBe` 1000
    it "input > 8" $ do
      let final = boot $ loadComputer tape [9]
      last (output final) `shouldBe` 1001
  describe "Day09 Examples" $ do
    it "example #1" $ do
      let tape  = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
          final = boot $ loadComputer tape [1]
      show (output final)
        `shouldBe` "[109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]"
    it "example #2" $ do
      let tape  = "1102,34915192,34915192,7,4,7,99,0"
          final = boot $ loadComputer tape [1]
      output final `shouldBe` [1219070632396864]
    it "example #3" $ do
      let tape  = "104,1125899906842624,99"
          final = boot $ loadComputer tape [1]
      output final `shouldBe` [1125899906842624]
