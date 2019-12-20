module Day18Spec.Helper where

import Helper
import Test.Hspec hiding (example)

example :: Solver -> String -> String -> String -> SpecWith (Arg Expectation)
example = solveExample "day18"
