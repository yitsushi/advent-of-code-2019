module Main where

import Data.Semigroup ((<>))
import Lib
import Options.Applicative
import qualified Solver

data Options =
  Options
    { day :: Int
    , part :: Int
    , input :: String
    }

options :: Parser Options
options =
  Options <$>
  option auto (long "day" <> help "Number of the Day [1..25]; not padded") <*>
  option
    auto
    (long "part" <> help "First or second part" <> showDefault <> value 1) <*>
  strOption
    (long "input" <>
     help "Input file; '-' -> stdin" <> showDefault <> value "No Input")

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> header "Advent of Code 2019" <> progDesc "Haskell this year")

main :: IO ()
main = solver =<< execParser opts

solver :: Options -> IO ()
solver (Options oDay part oInput) = do
  input <- Solver.readInput oInput
  putStrLn $ Solver.solve day part input
  where
    day =
      if oDay == 0
        then "demo"
        else lpad '0' 2 $ show oDay
