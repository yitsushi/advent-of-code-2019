module Day15.Lib where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace
import Intcode

type Area = Map.Map (Int, Int) Tile

visited :: Area -> (Int, Int) -> Bool
visited area position =
  case Map.lookup position area of
    Nothing -> False
    _ -> True

data Tile
  = Empty
  | Wall
  | OxygenSystem
  deriving (Show)

data Direction
  = North
  | East
  | South
  | West
  | Abort
  deriving (Show, Eq)

data ResponseCode
  = HitWall
  | Moved
  | Found
  deriving (Show, Eq)

readCode :: Int -> ResponseCode
readCode 0 = HitWall
readCode 1 = Moved
readCode 2 = Found

data Drone =
  Drone
    { droneProgram :: Computer
    , droneController :: ControllerFunction
    , mapAround :: Area
    , dronePosition :: (Int, Int)
    , lastMovement :: Maybe Direction
    }

type ControllerFunction = (Drone -> Maybe Direction)

responseCode :: Drone -> ResponseCode
responseCode = readCode . head . getOutput . droneProgram

move :: Drone -> Drone
move drone = drone {dronePosition = nextLocation}
  where
    (x, y) = dronePosition drone
    nextLocation =
      case lastMovement drone of
        Just North -> (x, y + 1)
        Just East -> (x + 1, y)
        Just South -> (x, y - 1)
        Just West -> (x - 1, y)
        _ -> error "Invalid move"
    nextValue =
      case responseCode drone of
        HitWall -> Wall
        Moved -> Empty
        Found -> OxygenSystem
    m = Map.insert nextLocation nextValue $ mapAround drone
    drone' =
      case nextValue of
        Wall -> drone {mapAround = m}
        _ -> drone {mapAround = m, dronePosition = nextLocation}

executeDrone :: Drone -> Drone
executeDrone drone
  | trace (show $ getOutput $ droneProgram drone) False = undefined
  | isTerminated (droneProgram drone) = drone
  | otherwise = executeDrone droneNext
  | isNothing response = drone
  where
    currentState = droneProgram drone
    drone' = move drone
    response = droneController drone' drone'
    input =
      case response of
        Nothing -> 99
        Just North -> 1
        Just East -> 4
        Just South -> 2
        Just West -> 3
        _ -> error "Invalid direction"
    program = execute $ feedInput input $ resetOutput currentState
    droneNext = drone' {droneProgram = program, lastMovement = response}

newDrone :: Tape -> ControllerFunction -> Drone
newDrone tape controller =
  Drone
    { droneProgram = execute (Computer tape [1] 0 [] 0)
    , droneController = controller
    , mapAround = Map.singleton (0, 0) Empty
    , dronePosition = (0, 0)
    , lastMovement = Just North
    }
