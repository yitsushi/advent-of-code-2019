module Intcode where

import Lib (splitOn)

parse :: String -> Tape
parse = map read . splitOn ','

type Tape = [Int]

type InputTape = [Int]

type OutputTape = [Int]

type Head = Int

data Computer =
  Computer Tape InputTape Head OutputTape
  deriving (Show, Eq)

data ParameterMode
  = Pointer
  | Immediate
  deriving (Show, Eq)

readMode :: Int -> ParameterMode
readMode 0 = Pointer
readMode 1 = Immediate
readMode _ = error "Unknown mode"

readOpCode :: Int -> (Int, [ParameterMode])
readOpCode op = (opCode, [p1mode, p2mode, p3mode])
  where
    opCode = mod op 100
    calc = readMode . flip mod 10 . div op
    p1mode = calc 100
    p2mode = calc 1000
    p3mode = calc 10000

resolveValue :: Tape -> (ParameterMode, Int) -> Int
resolveValue tape (Pointer, value) = tape !! value
resolveValue tape (Immediate, value) = value

resolveValues :: Tape -> [ParameterMode] -> Head -> Int -> [Int]
resolveValues tape pModes phead number =
  map (resolveValue tape) $ zip pModes (take number $ drop (phead + 1) tape)

replaceRegister :: Tape -> Int -> Int -> Tape
replaceRegister tape position value = before ++ value : after
  where
    before = take position tape
    after = drop (position + 1) tape

addCommand :: Computer -> [ParameterMode] -> Computer
addCommand (Computer tape input phead output) pModes =
  Computer tape' input (phead + 4) output
  where
    [p1, p2] = resolveValues tape pModes phead 2
    destination = tape !! (phead + 3)
    tape' = replaceRegister tape destination (p1 + p2)

mulCommand :: Computer -> [ParameterMode] -> Computer
mulCommand (Computer tape input phead output) pModes =
  Computer tape' input (phead + 4) output
  where
    [p1, p2] = resolveValues tape pModes phead 2
    destination = tape !! (phead + 3)
    tape' = replaceRegister tape destination (p1 * p2)

inputCommand :: Computer -> [ParameterMode] -> Computer
inputCommand (Computer tape input phead output) pModes =
  Computer tape' input (phead + 2) output
  where
    destination = tape !! (phead + 1)
    tape' = replaceRegister tape destination (head input)

outputCommand :: Computer -> [ParameterMode] -> Computer
outputCommand (Computer tape input phead output) pModes =
  Computer tape input (phead + 2) output'
  where
    output' = output ++ [resolveValue tape (Pointer, tape !! (phead + 1))]

execute :: Computer -> Computer
execute (Computer tape input phead output)
  | op == 1 = execute $ addCommand (Computer tape input phead output) pmodes
  | op == 2 = execute $ mulCommand (Computer tape input phead output) pmodes
  | op == 3 = execute $ inputCommand (Computer tape input phead output) pmodes
  | op == 4 = execute $ outputCommand (Computer tape input phead output) pmodes
  | op == 99 = Computer tape input phead output
  | otherwise = error ("Unknown operator: " ++ show op)
  where
    (op, pmodes) = readOpCode $ tape !! phead

valueInRegister :: Computer -> Int -> Int
valueInRegister (Computer tape _ _ _) pos = tape !! pos

getNounVerb :: Computer -> Tape
getNounVerb (Computer tape _ _ _) = take 2 $ tail tape

getOutput :: Computer -> OutputTape
getOutput (Computer _ _ _ output) = output
