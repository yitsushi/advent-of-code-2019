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

preprocessorAddMul :: Tape -> [ParameterMode] -> Int -> (Int, Int, Tape, Tape)
preprocessorAddMul tape pModes head = (p1, p2, before, after)
  where
    [p1, p2] =
      map (resolveValue tape) $ zip pModes (take 2 $ drop (head + 1) tape)
    destination = tape !! (head + 3)
    before = take destination tape
    after = drop (destination + 1) tape

addCommand :: Computer -> [ParameterMode] -> Computer
addCommand (Computer tape input head output) pModes =
  Computer tape' input (head + 4) output
  where
    (p1, p2, before, after) = preprocessorAddMul tape pModes head
    tape' = before ++ [p1 + p2] ++ after

mulCommand :: Computer -> [ParameterMode] -> Computer
mulCommand (Computer tape input head output) pModes =
  Computer tape' input (head + 4) output
  where
    (p1, p2, before, after) = preprocessorAddMul tape pModes head
    tape' = before ++ [p1 * p2] ++ after

execute :: Computer -> Computer
execute (Computer tape input head output)
  | op == 1 = execute $ addCommand (Computer tape input head output) pmodes
  | op == 2 = execute $ mulCommand (Computer tape input head output) pmodes
  | op == 99 = Computer tape input head output
  | otherwise = error "Unknown operator"
  where
    (op, pmodes) = readOpCode $ tape !! head

valueInRegister :: Computer -> Int -> Int
valueInRegister (Computer tape _ _ _) pos = tape !! pos

getNounVerb :: Computer -> Tape
getNounVerb (Computer tape _ _ _) = take 2 $ tail tape
