module IntcodeMachine
  ( Computer(..)
  , newComputer
  , boot
  , loadComputer
  , Status(..)
  , parse
  ) where

import Control.Monad.State.Lazy
import Lib

type Tape = [Int]

parse :: String -> Tape
parse = map read . splitOn ','

data Status
  = Normal
  | Term
  | Waiting
  deriving (Eq, Show)

data Computer =
  Computer
    { memory :: Tape
    , ip :: Int
    , input :: Tape
    , output :: Tape
    , relativeBase :: Int
    , status :: Status
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
  if length mem < position
    then return 0
    else return (mem !! position)

writeAddress :: Int -> Int -> ComputerState ()
writeAddress p v = do
  memory <- gets memory
  let newMemory =
        if length memory < (p + 1)
          then do
            let more = (p + 1) - length memory
            memory ++ replicate more 0
          else memory
  modify $ \comp ->
    comp {memory = take p newMemory ++ [v] ++ drop (p + 1) newMemory}

write :: ParameterMode -> Int -> ComputerState ()
write p v = do
  offset <- gets relativeBase
  addr <- readAndConsume
  case p of
    Pointer -> writeAddress addr v
    Immediate -> error "Not supported operation: Immediate write"
    Relative -> writeAddress (addr + offset) v

fetchOp :: Int -> Op
fetchOp value =
  case code of
    1 -> Add (p1, p2, p3)
    2 -> Mul (p1, p2, p3)
    3 -> Input p1
    4 -> Output p1
    5 -> JumpIfTrue (p1, p2)
    6 -> JumpIfFalse (p1, p2)
    7 -> Less (p1, p2, p3)
    8 -> Equal (p1, p2, p3)
    9 -> SetBase p1
    99 -> Terminate
    e -> error $ "Unknown OpCode" ++ show e
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
      Pointer -> readAddress pv
      Immediate -> return pv
      Relative -> readAddress (pv + offset)
  modify $ \c -> c {output = out ++ [v]}

execute :: ComputerState ()
execute = do
  h <- gets ip
  next <- fetchOp <$> readInstruction
  input <- gets input
  mem <- gets memory
  case next of
    Terminate -> modify $ \c -> c {status = Term}
    Input p1 ->
      if null input
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
    Pointer -> readAddress v
    Immediate -> return v
    Relative -> readAddress (v + offset)

boot :: Computer -> Computer
boot = execState execute

newComputer :: Tape -> [Int] -> Computer
newComputer tape inputs =
  Computer
    { memory = tape
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
