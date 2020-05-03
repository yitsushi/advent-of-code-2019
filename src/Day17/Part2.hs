module Day17.Part2 where

import           Data.Char
import           Data.List
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Day17.Lib
import           IntcodeMachine

{- I think I know what's the problem :/
    I assume the solution can contain programs
    that walks off the scaffold path, but still
    visits all of them

    Nope... My code does not handle if the solution
    starts with AABC or ABBC or ABCC or any
    repeated pattern.

    Yep, that was the problem XD
-}
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
solve input      = case plan of
  Just p -> show $ extractScore $ executeMachine $ clone machine p
  _      -> "No solution! :("
 where
  machine      = newMachine input
  path         = screenToSteps $ machineScreen $ executeMachine machine
  plan         = generatePossibleRoutines path
  extractScore = last . output . machineProgram
  clone :: Machine -> InputSequence -> Machine
  clone ma is = ma { machineProgram = comp }
   where
    comp =
      writeMemory 0 2 $ feedInputs (machineProgram ma) (renderInputSequence is)

--solve input = unlines $ renderScreen machine
data Movement
  = Lft
  | Rght
  | Forward
  deriving (Eq)

class CommandSequenceElem a where
  toIntValues :: a -> [Int]

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
renderInputSequence InputSequence { routines = r, aSequence = a, bSequence = b, cSequence = c }
  = toCommandSequence r
    ++ concatMap toCommandSequence [a, b, c]
    ++ [ord 'n', 10]

type RoutineSequence = [RoutineCommand]

instance CommandSequenceElem RoutineCommand where
  toIntValues L                = [76]
  toIntValues R                = [82]
  toIntValues (ForwardValue v) = map ord (show v)

instance CommandSequenceElem Routine where
  toIntValues A = [65]
  toIntValues B = [66]
  toIntValues C = [67]

pathToSequence :: [Movement] -> RoutineSequence
pathToSequence = map convert . group
 where
  convert [Lft ] = L
  convert [Rght] = R
  convert xs     = ForwardValue (length xs)

type Possibility = ([Movement], [Movement], [Movement])

toCommandSequence :: CommandSequenceElem a => [a] -> [Int]
toCommandSequence xs =
  (Data.List.intercalate [44] . map toIntValues) xs ++ [10]

generatePossibleRoutines :: [Movement] -> Maybe InputSequence
generatePossibleRoutines path
  | (not . null) allPossibilities = Just
    (toInputSequence $ head allPossibilities)
  | otherwise = Nothing
 where
  allPossibilities = filter filterRoutes $ noRemainders possibleABC
  filterRoutes :: ([Routine], Possibility) -> Bool
  filterRoutes (r, (a, b, c)) =
    ((\c -> 1 < c && c <= 21) . length . toCommandSequence) r
  toInputSequence :: ([Routine], Possibility) -> InputSequence
  toInputSequence (r, (a, b, c)) = InputSequence { routines  = r
                                                 , aSequence = pathToSequence a
                                                 , bSequence = pathToSequence b
                                                 , cSequence = pathToSequence c
                                                 }
  gen =
    filter (goodLength . length . toCommandSequence . pathToSequence) . inits
    where goodLength x = 1 < x && x <= 21
  dropPart a = drop (length a)
  possibleA  = gen path
  possibleAB = concatMap (build path) possibleA
   where
    build p a | take (length a) p == a = build (dropPart a p) a
              | otherwise              = zip (repeat a) (gen p)
  possibleABC = map repack $ concatMap (build path) possibleAB
   where
    repack ((a, b), c) = (a, b, c)
    build p (a, b) | take (length a) p == a = build (dropPart a p) (a, b)
                   | take (length b) p == b = build (dropPart b p) (a, b)
                   | otherwise              = zip (repeat (a, b)) (gen p)
  noRemainders :: [Possibility] -> [([Routine], Possibility)]
  noRemainders = map unwrap . filter removeNothing . map
    (\x -> (converted x, x))
   where
    converted = convert path
    removeNothing (r, _) = isJust r
    unwrap (Just r, p) = (r, p)
  convert :: [Movement] -> Possibility -> Maybe [Routine]
  convert [] _ = Just []
  convert p (a, b, c)
    | a == aLenList = (++) <$> Just [A] <*> convert
      (drop (length a) p)
      (a, b, c)
    | b == bLenList = (++) <$> Just [B] <*> convert
      (drop (length b) p)
      (a, b, c)
    | c == cLenList = (++) <$> Just [C] <*> convert
      (drop (length c) p)
      (a, b, c)
    | otherwise = Nothing
   where
    aLenList = take (length a) p
    bLenList = take (length b) p
    cLenList = take (length c) p

extractRobot :: Screen -> ((Int, Int), Direction)
extractRobot = extract . head . dropWhile catchRobot . Map.toList
 where
  catchRobot (_, tile) = case tile of
    Robot d -> False
    _       -> True
  extract (pos, Robot dir) = (pos, dir)

instance Show Movement where
  show Lft     = "L"
  show Rght    = "R"
  show Forward = "_"

turnDirection :: Direction -> Movement -> Direction
turnDirection North Lft  = West
turnDirection West  Lft  = South
turnDirection South Lft  = East
turnDirection East  Lft  = North
turnDirection South Rght = West
turnDirection East  Rght = South
turnDirection North Rght = East
turnDirection West  Rght = North
turnDirection dir   _    = dir

moveNext :: ((Int, Int), Direction) -> ((Int, Int), Direction)
moveNext ((x, y), North) = ((x, y - 1), North)
moveNext ((x, y), South) = ((x, y + 1), South)
moveNext ((x, y), East ) = ((x + 1, y), East)
moveNext ((x, y), West ) = ((x - 1, y), West)
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
    | isValid path pos (lft facing)  = Lft : consume (pos, lft facing)
    | isValid path pos (rght facing) = Rght : consume (pos, rght facing)
    | otherwise                      = []
