module Day07.Part2
  ( solve
  ) where

import Data.List
import Intcode

cycleExecute :: [Computer] -> [Computer]
cycleExecute (Computer tape input phead output:rest)
  | all isTerminated (current : rest) = current : rest
  | otherwise = cycleExecute (rest ++ [current])
  where
    lastOutput = getOutput $ last rest
    nextInput
      | null lastOutput = [0]
      | otherwise = lastOutput
    nextMachine = Computer tape (input ++ nextInput) phead []
    current = execute nextMachine

solve :: String -> String
solve input = (show . maximum . map oneCycle . permutations) [5 .. 9]
  where
    tape = parse input
    engines sequence = [Computer tape [x] 0 [] | x <- sequence]
    oneCycle = head . getOutput . head . cycleExecute . engines
