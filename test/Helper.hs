module Helper where

import System.FilePath ((</>))
import Test.Hspec

loadFixture :: String -> String -> IO String
loadFixture section file = readFile path
  where
    path = "test" </> "fixtures" </> section </> file

type IntTitleizer = (Int -> String)

type ExamplesForSolve = (Int, (String, String))

type Solver = (String -> String)

solveExecutor ::
     IntTitleizer -> Solver -> ExamplesForSolve -> SpecWith (Arg Expectation)
solveExecutor title solve (i, (input, expected)) =
  it (title i) $ solve input `shouldBe` expected
