module Intcode where

import Lib (splitOn)

parse :: String -> Tape
parse = map read . splitOn ','

type Tape = [Int]

type InputTape = [Int]

type OutputTape = [Int]

type Head = Int

data Computer =
  Computer Tape InputTape Head OutputTape Int
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
addCommand (Computer tape input phead output rbase) pModes =
  Computer tape' input (phead + 4) output rbase
  where
    [p1, p2] = resolveValues tape pModes phead 2
    destination = tape !! (phead + 3)
    tape' = replaceRegister tape destination (p1 + p2)

mulCommand :: Computer -> [ParameterMode] -> Computer
mulCommand (Computer tape input phead output rbase) pModes =
  Computer tape' input (phead + 4) output rbase
  where
    [p1, p2] = resolveValues tape pModes phead 2
    destination = tape !! (phead + 3)
    tape' = replaceRegister tape destination (p1 * p2)

inputCommand :: Computer -> [ParameterMode] -> Computer
inputCommand (Computer tape input phead output rbase) pModes =
  Computer tape' input' (phead + 2) output rbase
  where
    destination = tape !! (phead + 1)
    (inputValue, input') = (head input, tail input)
    tape' = replaceRegister tape destination inputValue

outputCommand :: Computer -> [ParameterMode] -> Computer
outputCommand (Computer tape input phead output rbase) pModes =
  Computer tape input (phead + 2) output' rbase
  where
    p1 = resolveValues tape pModes phead 1
    output' = output ++ p1

jumpIfTrueCommand :: Computer -> [ParameterMode] -> Computer
jumpIfTrueCommand (Computer tape input phead output rbase) pModes =
  Computer tape input phead' output rbase
  where
    [p1, p2] = resolveValues tape pModes phead 2
    phead' =
      if p1 /= 0
        then p2
        else phead + 3

jumpIfFalseCommand :: Computer -> [ParameterMode] -> Computer
jumpIfFalseCommand (Computer tape input phead output rbase) pModes =
  Computer tape input phead' output rbase
  where
    [p1, p2] = resolveValues tape pModes phead 2
    phead' =
      if p1 == 0
        then p2
        else phead + 3

lessThanCommand :: Computer -> [ParameterMode] -> Computer
lessThanCommand (Computer tape input phead output rbase) pModes =
  Computer tape' input (phead + 4) output rbase
  where
    [p1, p2] = resolveValues tape pModes phead 2
    value =
      if p1 < p2
        then 1
        else 0
    destination = tape !! (phead + 3)
    tape' = replaceRegister tape destination value

equalsCommand :: Computer -> [ParameterMode] -> Computer
equalsCommand (Computer tape input phead output rbase) pModes =
  Computer tape' input (phead + 4) output rbase
  where
    [p1, p2] = resolveValues tape pModes phead 2
    value =
      if p1 == p2
        then 1
        else 0
    destination = tape !! (phead + 3)
    tape' = replaceRegister tape destination value

execute :: Computer -> Computer
execute (Computer tape input phead output rbase)
  | op == 1 = execute $ addCommand comp pmodes
  | op == 2 = execute $ mulCommand comp pmodes
  | op == 3 && null input = Computer tape input phead output rbase
  | op == 3 = execute $ inputCommand comp pmodes
  | op == 4 = execute $ outputCommand comp pmodes
  | op == 5 = execute $ jumpIfTrueCommand comp pmodes
  | op == 6 = execute $ jumpIfFalseCommand comp pmodes
  | op == 7 = execute $ lessThanCommand comp pmodes
  | op == 8 = execute $ equalsCommand comp pmodes
  | op == 99 = Computer tape input phead output rbase
  | otherwise = error ("Unknown operator: " ++ show op)
  where
    (op, pmodes) = readOpCode $ tape !! phead
    comp = Computer tape input phead output rbase

valueInRegister :: Computer -> Int -> Int
valueInRegister (Computer tape _ _ _ _) pos = tape !! pos

getNounVerb :: Computer -> Tape
getNounVerb (Computer tape _ _ _ _) = take 2 $ tail tape

getOutput :: Computer -> OutputTape
getOutput (Computer _ _ _ output _) = output

isTerminated :: Computer -> Bool
isTerminated (Computer tape _ phead _ _) = 99 == (tape !! phead)
