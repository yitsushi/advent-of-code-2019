module Day04Spec.LibSpec where

import           Control.Monad
import           Day04.Lib
import           Helper
import           Test.Hspec

validPasswordExamples = [(111111, True), (223450, False), (123789, False)]

hasDoubleExamples = [(112233, True), (123444, False), (111122, True)]

title :: Int -> Bool -> String
title v True  = show v ++ " is Valid"
title v False = show v ++ " is Not Valid"

executor :: (Int -> Bool) -> (Int, Bool) -> SpecWith (Arg Expectation)
executor solver (input, expected) =
  it (title input expected) $ solver input `shouldBe` expected

spec :: Spec
spec = do
  describe "validPassword"
    $ forM_ validPasswordExamples (executor validPassword)
  describe "hasDouble" $ forM_ hasDoubleExamples (executor (hasDouble . show))
