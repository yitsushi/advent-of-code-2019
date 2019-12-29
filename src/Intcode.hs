module Intcode where

import Lib (splitOn)

parse :: String -> Tape
parse = map read . splitOn ','

type Tape = [Int]

type InputTape = [Int]

type OutputTape = [Int]

type Head = Int

type RelativeBase = Int

data Computer =
  Computer Tape InputTape Head OutputTape RelativeBase
  deriving (Show, Eq)

data ParameterMode
  = Pointer
  | Immediate
  | Relative
  deriving (Show, Eq)

readMode :: Int -> ParameterMode
readMode 0 = Pointer
readMode 1 = Immediate
readMode 2 = Relative
readMode _ = error "Unknown mode"

readOpCode :: Int -> (Int, [ParameterMode])
readOpCode op = (opCode, [p1mode, p2mode, p3mode])
  where
    opCode = mod op 100
    calc = readMode . flip mod 10 . div op
    p1mode = calc 100
    p2mode = calc 1000
    p3mode = calc 10000

resolveValue :: Tape -> RelativeBase -> (ParameterMode, Int) -> Int
resolveValue tape _ (Pointer, value) =
  valueInRegister (Computer tape [] 0 [] 0) value
resolveValue tape _ (Immediate, value) = value
resolveValue tape rbase (Relative, value) =
  valueInRegister (Computer tape [] 0 [] 0) (rbase + value)

resolveValues :: Tape -> RelativeBase -> [ParameterMode] -> Head -> Int -> [Int]
resolveValues tape rbase pModes phead number =
  map (resolveValue tape rbase) $
  zip pModes (take number $ drop (phead + 1) tape)

replaceRegister :: Tape -> Int -> Int -> Tape
replaceRegister tape position value
  | length tape < (position + 1) =
    replaceRegister
      (tape ++ replicate ((position + 1) - length tape) 0)
      position
      value
  | otherwise = before ++ value : after
  where
    before = take position tape
    after = drop (position + 1) tape

valueInRegister :: Computer -> Int -> Int
valueInRegister (Computer tape _ _ _ _) pos
  | length tape < pos = 0
  | otherwise = tape !! pos

calculateDestination :: Computer -> [ParameterMode] -> Int -> Int
calculateDestination comp@(Computer tape input phead output rbase) pModes shift
  | pMode == Pointer = pointerValue
  | pMode == Relative = pointerValue + rbase
  | otherwise =
    error
      ("Destination is should be Pointer or Relative, but it is: " ++ show pMode)
  where
    pMode = pModes !! (shift - 1)
    pointerValue = valueInRegister comp (phead + shift)

addCommand :: Computer -> [ParameterMode] -> Computer
addCommand comp@(Computer tape input phead output rbase) pModes =
  Computer tape' input (phead + 4) output rbase
  where
    [p1, p2] = resolveValues tape rbase pModes phead 2
    destination = calculateDestination comp pModes 3
    tape' = replaceRegister tape destination (p1 + p2)

mulCommand :: Computer -> [ParameterMode] -> Computer
mulCommand comp@(Computer tape input phead output rbase) pModes =
  Computer tape' input (phead + 4) output rbase
  where
    [p1, p2] = resolveValues tape rbase pModes phead 2
    destination = calculateDestination comp pModes 3
    tape' = replaceRegister tape destination (p1 * p2)

inputCommand :: Computer -> [ParameterMode] -> Computer
inputCommand comp@(Computer tape input phead output rbase) pModes =
  Computer tape' input' (phead + 2) output rbase
  where
    destination = calculateDestination comp pModes 1
    (inputValue, input') = (head input, tail input)
    tape' = replaceRegister tape destination inputValue

outputCommand :: Computer -> [ParameterMode] -> Computer
outputCommand (Computer tape input phead output rbase) pModes =
  Computer tape input (phead + 2) output' rbase
  where
    p1 = resolveValues tape rbase pModes phead 1
    output' = output ++ p1

jumpIfTrueCommand :: Computer -> [ParameterMode] -> Computer
jumpIfTrueCommand (Computer tape input phead output rbase) pModes =
  Computer tape input phead' output rbase
  where
    [p1, p2] = resolveValues tape rbase pModes phead 2
    phead' =
      if p1 /= 0
        then p2
        else phead + 3

jumpIfFalseCommand :: Computer -> [ParameterMode] -> Computer
jumpIfFalseCommand (Computer tape input phead output rbase) pModes =
  Computer tape input phead' output rbase
  where
    [p1, p2] = resolveValues tape rbase pModes phead 2
    phead' =
      if p1 == 0
        then p2
        else phead + 3

lessThanCommand :: Computer -> [ParameterMode] -> Computer
lessThanCommand comp@(Computer tape input phead output rbase) pModes =
  Computer tape' input (phead + 4) output rbase
  where
    [p1, p2] = resolveValues tape rbase pModes phead 2
    value =
      if p1 < p2
        then 1
        else 0
    destination = calculateDestination comp pModes 3
    tape' = replaceRegister tape destination value

equalsCommand :: Computer -> [ParameterMode] -> Computer
equalsCommand comp@(Computer tape input phead output rbase) pModes =
  Computer tape' input (phead + 4) output rbase
  where
    [p1, p2] = resolveValues tape rbase pModes phead 2
    value =
      if p1 == p2
        then 1
        else 0
    destination = calculateDestination comp pModes 3
    tape' = replaceRegister tape destination value

adjustBaseCommand :: Computer -> [ParameterMode] -> Computer
adjustBaseCommand (Computer tape input phead output rbase) pModes =
  Computer tape input (phead + 2) output (rbase + p1)
  where
    [p1] = resolveValues tape rbase pModes phead 1

execute :: Computer -> Computer
execute comp@(Computer tape input phead output rbase)
  | op == 1 = execute $ addCommand comp pmodes
  | op == 2 = execute $ mulCommand comp pmodes
  | op == 3 && null input = comp
  | op == 3 = execute $ inputCommand comp pmodes
  | op == 4 = execute $ outputCommand comp pmodes
  | op == 5 = execute $ jumpIfTrueCommand comp pmodes
  | op == 6 = execute $ jumpIfFalseCommand comp pmodes
  | op == 7 = execute $ lessThanCommand comp pmodes
  | op == 8 = execute $ equalsCommand comp pmodes
  | op == 9 = execute $ adjustBaseCommand comp pmodes
  | op == 99 = Computer tape input phead output rbase
  | otherwise = error ("Unknown operator: " ++ show op)
  where
    (op, pmodes) = readOpCode $ tape !! phead

getNounVerb :: Computer -> Tape
getNounVerb (Computer tape _ _ _ _) = take 2 $ tail tape

getOutput :: Computer -> OutputTape
getOutput (Computer _ _ _ output _) = output

isTerminated :: Computer -> Bool
isTerminated (Computer tape _ phead _ _) = 99 == (tape !! phead)

feedInput :: Int -> Computer -> Computer
feedInput value (Computer tape input phead output rbase) =
  Computer tape (input ++ [value]) phead output rbase

resetOutput :: Computer -> Computer
resetOutput (Computer tape input phead _ rbase) =
  Computer tape input phead [] rbase

loadComputer :: String -> [Int] -> Computer
loadComputer tapeString inputs = do
  let tape = parse tapeString
  Computer tape inputs 0 [] 0
