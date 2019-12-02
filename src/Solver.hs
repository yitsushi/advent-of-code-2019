module Solver where

import Control.Monad.IO.Class (liftIO)
import Data.Text
import qualified Day01.Main
import qualified Demo.Main
import System.Directory

-- TODO: Make it dynamic somehow?
solve :: String -> (Int -> String -> String)
solve "demo" = Demo.Main.solve
solve "01" = Day01.Main.solve
solve day = notImplementedYet day

notImplementedYet :: String -> Int -> String -> String
notImplementedYet day _ _ = "\n\n-- [Day" ++ day ++ "] Not Implemented Yet!\n"

readInput :: String -> IO String
readInput f = do
  dfe <- liftIO $ doesFileExist (f :: FilePath)
  if dfe
    then readFile f
    else if f /= "-"
           then pure f
           else getContents
