module Solver where

import Control.Monad.IO.Class (liftIO)
import Data.Text
import qualified Day01.Main
import qualified Day02.Main
import qualified Day03.Main
import qualified Demo.Main
import System.Directory

-- | Router for days
--
-- __TODO:__ Make it dynamic somehow?
solve :: String -> (Int -> String -> String)
solve "demo" = Demo.Main.solve
solve "01" = Day01.Main.solve
solve "02" = Day02.Main.solve
solve "03" = Day03.Main.solve
solve day = notImplementedYet day

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
  if dfe
    then readFile f
    else if f /= "-"
           then pure f
           else getContents
