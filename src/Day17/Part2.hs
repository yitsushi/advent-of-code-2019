module Day17.Part2
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import           Day17.Lib
import           IntcodeMachine

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
solve input = show path
--solve input = unlines $ renderScreen machine
  where
    machine = executeMachine $ newMachine input
    path = screenToSteps $ machineScreen machine

extractRobot :: Screen -> ((Int, Int), Direction)
extractRobot = extract . head . dropWhile catchRobot . Map.toList
  where
    catchRobot (_, tile) =
      case tile of
        Robot d -> False
        _       -> True
    extract (pos, Robot dir) = (pos, dir)

data Movement
  = Lft
  | Rght
  | Forward

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
