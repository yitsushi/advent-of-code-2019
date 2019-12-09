module IntcodeTests.ExecuteSpec where

import Intcode
import Test.Hspec

spec :: Spec
spec =
  describe "execute" $ do
    describe "Day02 Examples" $ do
      it "example #1" $
        execute (Computer [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [] 0 [] 0) `shouldBe`
        Computer [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50] [] 8 [] 0
      it "example #2" $
        execute (Computer [1, 0, 0, 0, 99] [] 0 [] 0) `shouldBe`
        Computer [2, 0, 0, 0, 99] [] 4 [] 0
      it "example #3" $
        execute (Computer [2, 3, 0, 3, 99] [] 0 [] 0) `shouldBe`
        Computer [2, 3, 0, 6, 99] [] 4 [] 0
      it "example #4" $
        execute (Computer [2, 4, 4, 5, 99, 0] [] 0 [] 0) `shouldBe`
        Computer [2, 4, 4, 5, 99, 9801] [] 4 [] 0
      it "example #5" $
        execute (Computer [1, 1, 1, 4, 99, 5, 6, 0, 99] [] 0 [] 0) `shouldBe`
        Computer [30, 1, 1, 4, 2, 5, 6, 0, 99] [] 8 [] 0
    describe "Day05 Examples" $ do
      let tape =
            parse
              "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
      it "input < 8" $
        last (getOutput (execute (Computer tape [7] 0 [999] 0))) `shouldBe` 999
      it "input == 8" $
        last (getOutput (execute (Computer tape [8] 0 [999] 0))) `shouldBe` 1000
      it "input > 8" $
        last (getOutput (execute (Computer tape [9] 0 [999] 0))) `shouldBe` 1001
    describe "Day09 Examples" $ do
      it "example #1" $
        getOutput
          (execute
             (Computer
                [ 109
                , 1
                , 204
                , -1
                , 1001
                , 100
                , 1
                , 100
                , 1008
                , 100
                , 16
                , 101
                , 1006
                , 101
                , 0
                , 99
                ]
                [1]
                0
                []
                0)) `shouldBe`
        [ 109
        , 1
        , 204
        , -1
        , 1001
        , 100
        , 1
        , 100
        , 1008
        , 100
        , 16
        , 101
        , 1006
        , 101
        , 0
        , 99
        ]
      it "example #2" $
        getOutput
          (execute
             (Computer [1102, 34915192, 34915192, 7, 4, 7, 99, 0] [1] 0 [] 0)) `shouldBe`
        [1219070632396864]
      it "example #3" $
        getOutput (execute (Computer [104, 1125899906842624, 99] [1] 0 [] 0)) `shouldBe`
        [1125899906842624]
