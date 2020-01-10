module Day07.Part2
  ( solve
  ) where

import Data.List
import IntcodeMachine

cycleExecute :: [Computer] -> [Computer]
cycleExecute (computer:rest)
  | all isTerminated (current : rest) = current : rest
  | otherwise = cycleExecute (rest ++ [current])
  where
    lastOutput = output $ last rest
    nextInput
      | null lastOutput = [0]
      | otherwise = lastOutput
    nextMachine = wipeOutput $ feedInputs computer nextInput
    current = boot nextMachine

solve :: String -> String
solve input = (show . maximum . map oneCycle . permutations) [5 .. 9]
  where
    tape = parse input
    engines sequence = [newComputer tape [x] | x <- sequence]
    oneCycle = head . output . head . cycleExecute . engines
