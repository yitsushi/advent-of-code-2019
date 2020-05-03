module Solver where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text
import qualified Day01.Main
import qualified Day02.Main
import qualified Day03.Main
import qualified Day04.Main
import qualified Day05.Main
import qualified Day06.Main
import qualified Day07.Main
import qualified Day08.Main
import qualified Day09.Main
import qualified Day10.Main
import qualified Day11.Main
import qualified Day12.Main
import qualified Day13.Main
import qualified Day14.Main
import qualified Day15.Main
import qualified Day16.Main
import qualified Day17.Main
import qualified Day19.Main
import qualified Day22.Main
import qualified Day24.Main
import qualified Demo.Main
import           System.Directory

-- | Router for days
--
-- __TODO:__ Make it dynamic somehow?
solve :: String -> (Int -> String -> String)
solve "demo" = Demo.Main.solve
solve "01"   = Day01.Main.solve
solve "02"   = Day02.Main.solve
solve "03"   = Day03.Main.solve
solve "04"   = Day04.Main.solve
solve "05"   = Day05.Main.solve
solve "06"   = Day06.Main.solve
solve "07"   = Day07.Main.solve
solve "08"   = Day08.Main.solve
solve "09"   = Day09.Main.solve
solve "10"   = Day10.Main.solve
solve "11"   = Day11.Main.solve
solve "12"   = Day12.Main.solve
solve "13"   = Day13.Main.solve
solve "14"   = Day14.Main.solve
solve "15"   = Day15.Main.solve
solve "16"   = Day16.Main.solve
solve "17"   = Day17.Main.solve
solve "19"   = Day19.Main.solve
solve "22"   = Day22.Main.solve
solve "24"   = Day24.Main.solve
solve day    = notImplementedYet day

-- | Simple function for incpmplete days
notImplementedYet :: String -> Int -> String -> String
notImplementedYet day _ _ = "\n\n-- [Day" ++ day ++ "] Not Implemented Yet!\n"

-- | Reads input.
--
-- if __parameter__ is an existing file then reads its content
--
-- if __parameter__ value is __-__ then reads __StdInput__
--
-- otherwise use the value as it is.
readInput :: String -> IO String
readInput f = do
  dfe <- liftIO $ doesFileExist (f :: FilePath)
  if dfe then readFile f else if f /= "-" then pure f else getContents
