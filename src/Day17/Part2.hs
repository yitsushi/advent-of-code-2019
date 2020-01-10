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
solve input = unlines $ renderScreen machine
  where
    machine = executeMachine $ newMachine input

extractRobot :: Screen -> ((Int, Int), Direction)
extractRobot = extract . head . dropWhile catchRobot . Map.toList
  where
    catchRobot (_, tile) = case tile of
      Robot d -> False
      _       -> True
    extract (pos, Robot dir) = (pos, dir)

data Movement = Lft | Rght | Forward

screenToSteps :: Screen -> [Movement]
screenToSteps screen = undefined
  where
    path = Map.filter (== Scaffold) screen
    robot = extractRobot screen

turnOntoPath :: (Int, Int) -> Direction -> (Int, Int) -> Maybe Direction
turnOntoPath (robotX, robotY) facing (targetX, targetY)
  | x == 1 = Just South
  | x == (-1) = Just North
  | y == 1 = Just East
  | y == (-1) = Just West
  | otherwise = Nothing
  where
    x = targetX - robotX
    y = targetY - robotY
