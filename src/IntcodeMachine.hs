module IntcodeMachine
  ( Computer(..)
  , newComputer
  , boot
  , loadComputer
  , Status(..)
  , parse
  , cleanComputer
  , cleanComputerWithOutput
  , feedInput
  , feedInputs
  , wipeOutput
  , isTerminated
  , isWaiting
  , readMemory
  , writeMemory
  ) where

import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe
import           Lib

type Tape = [Int]

type Memory = HM.HashMap Int Int

parse :: String -> Tape
parse = map read . splitOn ','

data Status
  = Normal
  | Term
  | Waiting
  deriving (Eq, Show)

data Computer =
  Computer
    { memory       :: Memory
    , ip           :: Int
    , input        :: Tape
    , output       :: Tape
    , relativeBase :: Int
    , status       :: Status
    }

type ComputerState = State Computer

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

data Op
  = Add (ParameterMode, ParameterMode, ParameterMode)
  | Mul (ParameterMode, ParameterMode, ParameterMode)
  | Input ParameterMode
  | Output ParameterMode
  | JumpIfTrue (ParameterMode, ParameterMode)
  | JumpIfFalse (ParameterMode, ParameterMode)
  | Less (ParameterMode, ParameterMode, ParameterMode)
  | Equal (ParameterMode, ParameterMode, ParameterMode)
  | SetBase ParameterMode
  | Terminate
  deriving (Show)

readInstruction :: ComputerState Int
readInstruction = readAndConsume

readAndConsume :: ComputerState Int
readAndConsume = do
  h <- gets ip
  v <- readAddress h
  modify $ \c -> c {ip = h + 1}
  return v

readAddress :: Int -> ComputerState Int
readAddress position = do
  mem <- gets memory
  case HM.lookup position mem of
    Nothing    -> return 0
    Just value -> return value

writeAddress :: Int -> Int -> ComputerState ()
writeAddress p v = do
  mem <- gets memory
  modify $ \comp -> comp {memory = HM.insert p v mem}

write :: ParameterMode -> Int -> ComputerState ()
write p v = do
  offset <- gets relativeBase
  addr <- readAndConsume
  case p of
    Pointer   -> writeAddress addr v
    Immediate -> error "Not supported operation: Immediate write"
    Relative  -> writeAddress (addr + offset) v

fetchOp :: Int -> Op
fetchOp value =
  case code of
    1  -> Add (p1, p2, p3)
    2  -> Mul (p1, p2, p3)
    3  -> Input p1
    4  -> Output p1
    5  -> JumpIfTrue (p1, p2)
    6  -> JumpIfFalse (p1, p2)
    7  -> Less (p1, p2, p3)
    8  -> Equal (p1, p2, p3)
    9  -> SetBase p1
    99 -> Terminate
    e  -> error $ "Unknown OpCode" ++ show e
  where
    code = value `mod` 100
    calc = readMode . flip mod 10 . div value
    p1 = calc 100
    p2 = calc 1000
    p3 = calc 10000

ask :: ComputerState Int
ask = do
  inp <- gets input
  let (x, xs) = (head inp, tail inp)
  modify $ \c -> c {input = xs}
  return x

tell :: ParameterMode -> ComputerState ()
tell p = do
  out <- gets output
  offset <- gets relativeBase
  pv <- readAndConsume
  v <-
    case p of
      Pointer   -> readAddress pv
      Immediate -> return pv
      Relative  -> readAddress (pv + offset)
  modify $ \c -> c {output = out ++ [v]}

execute :: ComputerState ()
execute = do
  h <- gets ip
  next <- fetchOp <$> readInstruction
  inp <- gets input
  case next of
    Terminate -> modify $ \c -> c {status = Term}
    Input p1 ->
      if null inp
        then modify (\c -> c {status = Waiting, ip = h})
        else executeCommand (Input p1) *> execute
    op -> executeCommand op *> execute

arithmetic ::
     (Int -> Int -> Int)
  -> (ParameterMode, ParameterMode, ParameterMode)
  -> ComputerState ()
arithmetic f (p1, p2, p3) =
  liftM2 f (resolveParam p1) (resolveParam p2) >>= write p3

comparison ::
     (Int -> Int -> Bool)
  -> (ParameterMode, ParameterMode, ParameterMode)
  -> ComputerState ()
comparison c (p1, p2, p3) = do
  v1 <- resolveParam p1
  v2 <- resolveParam p2
  write p3 $
    if c v1 v2
      then 1
      else 0

jumpIf :: (Int -> Bool) -> (ParameterMode, ParameterMode) -> ComputerState ()
jumpIf check (p1, p2) = do
  x1 <- resolveParam p1
  x2 <- resolveParam p2
  when (check x1) $ modify $ \c -> c {ip = x2}

executeCommand :: Op -> ComputerState ()
executeCommand (Add ps) = arithmetic (+) ps
executeCommand (Mul ps) = arithmetic (*) ps
executeCommand (Input p1) = ask >>= write p1
executeCommand (Output p1) = tell p1
executeCommand (JumpIfTrue ps) = jumpIf (/= 0) ps
executeCommand (JumpIfFalse ps) = jumpIf (== 0) ps
executeCommand (Less ps) = comparison (<) ps
executeCommand (Equal ps) = comparison (==) ps
executeCommand (SetBase p1) = do
  base <- gets relativeBase
  change <- resolveParam p1
  modify $ \c -> c {relativeBase = base + change}
executeCommand a = error ("Unknown Op" ++ show a)

resolveParam :: ParameterMode -> ComputerState Int
resolveParam p1 = do
  offset <- gets relativeBase
  v <- readAndConsume
  case p1 of
    Pointer   -> readAddress v
    Immediate -> return v
    Relative  -> readAddress (v + offset)

boot :: Computer -> Computer
boot = execState execute

feedInput :: Computer -> Int -> Computer
feedInput computer value = do
  let currentInput = input computer
  computer {input = currentInput ++ [value]}

feedInputs :: Computer -> [Int] -> Computer
feedInputs computer value = do
  let currentInput = input computer
  computer {input = currentInput ++ value}

wipeOutput :: Computer -> Computer
wipeOutput computer = computer {output = []}

newComputer :: Tape -> [Int] -> Computer
newComputer tape inputs =
  Computer
    { memory = HM.fromList $ zip [0 ..] tape
    , ip = 0
    , input = inputs
    , output = []
    , relativeBase = 0
    , status = Normal
    }

loadComputer :: String -> [Int] -> Computer
loadComputer tapeString inputs = do
  let tape = parse tapeString
  newComputer tape inputs

cleanComputer :: Computer
cleanComputer = newComputer [] []

cleanComputerWithOutput :: [Int] -> Computer
cleanComputerWithOutput out =
  Computer
    { memory = HM.fromList []
    , ip = 0
    , input = []
    , output = out
    , relativeBase = 0
    , status = Normal
    }

writeMemory :: Int -> Int -> Computer -> Computer
writeMemory location value computer = computer {memory = newMemory}
  where
    newMemory = HM.insert location value (memory computer)

readMemory :: Int -> Computer -> Int
readMemory addr Computer {memory = mem} = fromMaybe 0 $ HM.lookup addr mem

isTerminated :: Computer -> Bool
isTerminated = (==) Term . status

isWaiting :: Computer -> Bool
isWaiting = (==) Waiting . status
