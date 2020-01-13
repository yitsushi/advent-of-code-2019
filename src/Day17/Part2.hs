module Day17.Part2 where

import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Day17.Lib
import           Debug.Trace
import           IntcodeMachine

debugReadExample :: String -> Screen
debugReadExample d = screen
  where
    rows = lines d
    screen = foldl fr (Map.singleton (0, 0) Empty) $ zip [0 ..] rows
      where
        fr s (y, row) = foldl fc s $ zip [0 ..] row
          where
            fc s (x, c) = Map.insert (x, y) value s
              where
                value
                  | c == '.' = Empty
                  | c == '#' = Scaffold
                  | c == '^' = Robot North
                  | c == 'v' = Robot South
                  | c == '>' = Robot East
                  | c == '<' = Robot West

{- Maybe approach
    1. Create a movement sequence based on the path
    2. Generate all possible init for A
    3. Drop each of them with length > 20
    4. Generate all possible init for B (drop length A)
    5. Drop each of them with length > 20
    6. Generate all possible init for C (drop length A + B)
    7. Drop each of them with length > 20
    8. Replace all appearance of A with RoutineA
    9. Replace all appearance of B with RoutineB
    10. Replace all appearance of C with RoutineC
    11. Remeaning list should contain only A, C and C
    12. Final list length < 20
    13. ???
    14. Profit
    Soon, but I'm not in the mood
-}
solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = case plan of
    Just p -> show $ output $ machineProgram $ executeMachine $ clone machine p
    _      -> "No solution! :("
  where
    machine = newMachine input
    path = screenToSteps $ machineScreen $ executeMachine machine
    plan = generatePossibleRoutines path
    clone :: Machine -> InputSequence -> Machine
    clone ma is = ma{machineProgram = comp}
      where comp = writeMemory 0 2 $ feedInputs (machineProgram ma) (renderInputSequence is)

--solve input = unlines $ renderScreen machine
data Movement
  = Lft
  | Rght
  | Forward
  deriving (Eq)

class CommandSequenceElem a where
  toIntValue :: a -> Int

data RoutineCommand
  = L
  | R
  | ForwardValue Int
  deriving (Show)

data Routine
  = A
  | B
  | C
  deriving (Show)

data InputSequence =
  InputSequence
    { routines  :: [Routine]
    , aSequence :: RoutineSequence
    , bSequence :: RoutineSequence
    , cSequence :: RoutineSequence
    }
  deriving (Show)

renderInputSequence :: InputSequence -> [Int]
renderInputSequence InputSequence { routines = r
                                  , aSequence = a
                                  , bSequence = b
                                  , cSequence = c
                                  } = toCommandSequence r ++ concatMap toCommandSequence [a, b, c]

type RoutineSequence = [RoutineCommand]

instance CommandSequenceElem RoutineCommand where
  toIntValue L                = 76
  toIntValue R                = 82
  toIntValue (ForwardValue v) = 48 + v

instance CommandSequenceElem Routine where
  toIntValue A = 65
  toIntValue B = 66
  toIntValue C = 67

pathToSequence :: [Movement] -> RoutineSequence
pathToSequence = map convert . group
  where
    convert [Lft]     = L
    convert [Rght]    = R
    convert [Forward] = ForwardValue 1
    convert xs        = ForwardValue (length xs)

type Possibility = ([Movement], [Movement], [Movement])

toCommandSequence :: CommandSequenceElem a => [a] -> [Int]
toCommandSequence xs = (Data.List.intersperse 44 . map toIntValue) xs ++ [10]

generatePossibleRoutines :: [Movement] -> Maybe InputSequence
generatePossibleRoutines path
  | (not . null) allPossibilities =
    Just (toInputSequence $ head allPossibilities)
  | otherwise = Nothing
  where
    allPossibilities = filter filterRoutes $ noRemainders possibleABC
    filterRoutes :: ([Routine], Possibility) -> Bool
    filterRoutes (r, (a, b, c)) =
      ((\c -> 1 < c && c <= 20) . length . toCommandSequence) r
    toInputSequence :: ([Routine], Possibility) -> InputSequence
    toInputSequence (r, (a, b, c)) =
      InputSequence
        { routines = r
        , aSequence = pathToSequence a
        , bSequence = pathToSequence b
        , cSequence = pathToSequence c
        }
    gen = filter (goodLength . length . pathToSequence) . inits
      where
        goodLength x = 0 < x && x <= 20
    possibleA = gen path
    possibleAB = concatMap build possibleA
      where
        build a = zip (repeat a) (gen $ drop (length a) path)
    possibleABC = map repack $ concatMap build possibleAB
      where
        repack ((a, b), c) = (a, b, c)
        genc a b = gen $ drop (length a + length b) path
        build (a, b) = zip (repeat (a, b)) (genc a b)
    noRemainders :: [Possibility] -> [([Routine], Possibility)]
    noRemainders =
      map unwrap . filter removeNothing . map (\x -> (converted x, x))
      where
        converted = convert path
        removeNothing (r, _) = isJust r
        unwrap (Just r, p) = (r, p)
    convert :: [Movement] -> Possibility -> Maybe [Routine]
    convert [] _ = Just []
    convert p (a, b, c)
      | a == aLenList =
        (++) <$> Just [A] <*> convert (drop (length a) p) (a, b, c)
      | b == bLenList =
        (++) <$> Just [B] <*> convert (drop (length b) p) (a, b, c)
      | c == cLenList =
        (++) <$> Just [C] <*> convert (drop (length c) p) (a, b, c)
      | otherwise = Nothing
      where
        aLenList = take (length a) p
        bLenList = take (length b) p
        cLenList = take (length c) p

extractRobot :: Screen -> ((Int, Int), Direction)
extractRobot = extract . head . dropWhile catchRobot . Map.toList
  where
    catchRobot (_, tile) =
      case tile of
        Robot d -> False
        _       -> True
    extract (pos, Robot dir) = (pos, dir)

instance Show Movement where
  show Lft     = "L"
  show Rght    = "R"
  show Forward = "_"

turnDirection :: Direction -> Movement -> Direction
turnDirection North Lft  = West
turnDirection West Lft   = South
turnDirection South Lft  = East
turnDirection East Lft   = North
turnDirection South Rght = West
turnDirection East Rght  = South
turnDirection North Rght = East
turnDirection West Rght  = North
turnDirection dir _      = dir

moveNext :: ((Int, Int), Direction) -> ((Int, Int), Direction)
moveNext ((x, y), North) = ((x, y - 1), North)
moveNext ((x, y), South) = ((x, y + 1), South)
moveNext ((x, y), East)  = ((x + 1, y), East)
moveNext ((x, y), West)  = ((x - 1, y), West)
moveNext _               = error "Something went wrong."

isValid :: Screen -> (Int, Int) -> Direction -> Bool
isValid screen (x, y) = check
  where
    comp :: (Int, Int) -> Bool
    comp pos = (==) (Just Scaffold) (Map.lookup pos screen)
    check :: Direction -> Bool
    check North = comp (x, y - 1)
    check South = comp (x, y + 1)
    check East  = comp (x + 1, y)
    check West  = comp (x - 1, y)

lft :: Direction -> Direction
lft East  = North
lft North = West
lft West  = South
lft South = East

rght :: Direction -> Direction
rght East  = South
rght North = East
rght West  = North
rght South = West

screenToSteps :: Screen -> [Movement]
screenToSteps screen = consume $ extractRobot screen
  where
    path = Map.filter (== Scaffold) screen
    consume :: ((Int, Int), Direction) -> [Movement]
    consume (pos, facing)
      | isValid path pos facing = Forward : consume (moveNext (pos, facing))
      | isValid path pos (lft facing) = Lft : consume (pos, lft facing)
      | isValid path pos (rght facing) = Rght : consume (pos, rght facing)
      | otherwise = []
