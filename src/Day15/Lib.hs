module Day15.Lib where

import           Data.Maybe
import           Data.Point
import qualified Data.PriorityQueue            as PQ
import qualified Data.WalkableMap              as WM
import           IntcodeMachine
import           Lib

type Area = WM.WalkableMap Tile

visited :: Area -> Point -> Bool
visited area position = WM.exists position area

generateNeightboors :: Point -> Int -> [Point]
generateNeightboors (px, py) r =
  [ (px, py + y) | y <- [-r .. r], y /= 0 ]
    ++ [ (px + x, py) | x <- [-r .. r], x /= 0 ]
    ++ [ (px + r, py + y) | y <- extra ]
    ++ [ (px - r, py + y) | y <- extra ]
    ++ [ (px + x, py + r) | x <- extra ]
    ++ [ (px + x, py - r) | x <- extra ]
  where extra = [-r .. (-1)] ++ [1 .. r]

data Tile
  = Empty
  | Wall
  | OxygenSystem
  | Unknown
  | Machine
  deriving (Show, Eq)

tileChar :: Tile -> Char
tileChar Empty        = '_'
tileChar Wall         = '#'
tileChar OxygenSystem = 'O'
tileChar Unknown      = ' '
tileChar Machine      = 'X'

data Direction
  = North
  | East
  | South
  | West
  | Abort
  deriving (Show, Eq)

directionCode :: Direction -> Int
directionCode North = 1
directionCode South = 2
directionCode West  = 3
directionCode East  = 4
directionCode _     = error "Invalid direction"

data ResponseCode
  = HitWall
  | Moved
  | Found
  deriving (Show, Eq)

readCode :: Int -> ResponseCode
readCode 0 = HitWall
readCode 1 = Moved
readCode 2 = Found

vectorToDirection :: (Int, Int) -> Direction
vectorToDirection (-1, 0 ) = West
vectorToDirection (1 , 0 ) = East
vectorToDirection (0 , -1) = North
vectorToDirection (0 , 1 ) = South
vectorToDirection (x , y ) = error ("Unknown direction: " ++ show (x, y))

data Drone =
  Drone
    { droneProgram :: Computer
    , droneController :: ControllerFunction
    , mapAround :: Area
    , dronePosition :: Point
    , lastMovement :: Direction
    , autoPilot :: [Point]
    }

type ControllerFunction = (Drone -> Either (Maybe Direction) [Point])

responseCode :: Drone -> ResponseCode
responseCode = readCode . head . output . droneProgram

move :: Drone -> Drone
move drone = drone'
 where
  (x, y)       = dronePosition drone
  nextLocation = case lastMovement drone of
    North -> (x, y - 1)
    East  -> (x + 1, y)
    South -> (x, y + 1)
    West  -> (x - 1, y)
    _     -> error "Invalid move"
  nextValue = case responseCode drone of
    HitWall -> Wall
    Moved   -> Empty
    Found   -> OxygenSystem
  m      = WM.update nextLocation nextValue $ mapAround drone
  drone' = case nextValue of
    Wall -> drone { mapAround = m }
    _    -> drone { mapAround = m, dronePosition = nextLocation }

draw :: Drone -> [String]
draw drone =
  WM.draw (WM.update (dronePosition drone) Machine $ mapAround drone) tileChar

executeDrone :: Drone -> Drone
executeDrone drone | isTerminated (droneProgram drone) = drone
                   | input == Abort                    = drone'
                   | otherwise                         = executeDrone droneNext
 where
  currentState   = droneProgram drone
  drone'         = move drone
  response       = droneController drone' drone'
  (input, pilot) = case response of
    Left a -> case a of
      Nothing        -> (Abort, [])
      Just direction -> (direction, [])
    Right a -> (vectorToDirection (head a <-> dronePosition drone'), tail a)
  program = boot $ wipeOutput $ feedInput currentState (directionCode input)
  droneNext =
    drone' { droneProgram = program, lastMovement = input, autoPilot = pilot }

newDrone :: String -> ControllerFunction -> Drone
newDrone tape controller = Drone
  { droneProgram    = boot (loadComputer tape [1])
  , droneController = controller
  , mapAround = (WM.fromList [((0, 0), Empty)] Unknown) { WM.obstacles = [Wall]
                                                        }
  , dronePosition   = (0, 0)
  , lastMovement    = North
  , autoPilot       = []
  }

exampleMap :: Direction -> WM.WalkableMap Tile
exampleMap East =
  WM.fromList (((0, 0), Empty) : [ ((-x, 0), Wall) | x <- [1 .. 10] ]) Unknown
exampleMap West =
  WM.fromList (((0, 0), Empty) : [ ((x, 0), Wall) | x <- [1 .. 10] ]) Unknown
exampleMap North =
  WM.fromList (((0, 0), Empty) : [ ((0, -y), Wall) | y <- [1 .. 10] ]) Unknown
exampleMap South =
  WM.fromList (((0, 0), Empty) : [ ((0, y), Wall) | y <- [1 .. 10] ]) Unknown

spreadOxygen :: Area -> Area
spreadOxygen area = area'
 where
  allOxygenZones = WM.findValue OxygenSystem area
  restrictedArea = area { WM.obstacles = [Wall, OxygenSystem] }
  edgeZones      = concatMap (`WM.neighbors` area) allOxygenZones
  area'          = foldl (\m x -> WM.update x OxygenSystem m) area edgeZones

drawExample :: Direction -> String
drawExample direction = unlines $ WM.draw (exampleMap direction) tileChar

-- code == Found = Left Nothing
controller :: Drone -> Either (Maybe Direction) [Point]
controller drone
  | not (null (autoPilot drone))
  = Right (autoPilot drone)
  | null nextStep
  = Left Nothing
  | length nextStep == 1
  = Left (Just (vectorToDirection (head nextStep <-> pos)))
  | otherwise
  = Right nextStep
 where
  code       = responseCode drone
  area       = mapAround drone
  pos        = dronePosition drone
  posItem    = PQ.Item { PQ.location = pos, PQ.score = 0, PQ.extra = [pos] }
  pathToNext = WM.pathToClosestValue posItem Unknown area
  nextStep   = case pathToNext of
    Nothing   -> []
    Just item -> reverse $ init $ PQ.extra item
