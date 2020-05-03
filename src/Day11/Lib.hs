module Day11.Lib where

import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import           IntcodeMachine
import           Lib

data Color
  = Black
  | White
  deriving (Show)

colorToInput :: Color -> Int
colorToInput Black = 0
colorToInput White = 1

inputToColor :: Int -> Color
inputToColor 0 = Black
inputToColor 1 = White
inputToColor _ = error "Unknown color"

data Direction
  = North
  | East
  | South
  | West
  deriving (Show)

clockwise :: Direction -> Direction
clockwise North = East
clockwise East  = South
clockwise South = West
clockwise West  = North

counterClockwise :: Direction -> Direction
counterClockwise North = West
counterClockwise West  = South
counterClockwise South = East
counterClockwise East  = North

turn :: Int -> Robot -> Robot
turn 1 (location, direction) = (location, counterClockwise direction)
turn 0 (location, direction) = (location, clockwise direction)
turn _ _                     = error "Something went horribly wrong!"

step :: Robot -> Robot
step ((x, y), North) = ((x, y + 1), North)
step ((x, y), East ) = ((x + 1, y), East)
step ((x, y), South) = ((x, y - 1), South)
step ((x, y), West ) = ((x - 1, y), West)

type Board = Map.Map (Int, Int) Color

type Robot = ((Int, Int), Direction)

colorAtPosition :: Board -> Robot -> Color
colorAtPosition board (location, _) =
  Maybe.fromMaybe Black $ Map.lookup location board

colorAt :: Board -> (Int, Int) -> Color
colorAt board location = Maybe.fromMaybe Black $ Map.lookup location board

paintAtPosition :: Board -> Robot -> Color -> Board
paintAtPosition board (location, _) color = Map.insert location color board

paint :: Robot -> Board -> Computer -> (Robot, Board, Computer)
paint robot board computer | isTerminated computer = (robot', board', computer)
                           | otherwise = paint robot' board' (boot computer')
 where
  o@(color : direction : xs) = output computer
  board'                     = paintAtPosition board robot (inputToColor color)
  robot'                     = (step . turn direction) robot
  nextInput                  = colorToInput $ colorAtPosition board' robot'
  computer'                  = (wipeOutput . feedInput computer) nextInput

render :: Board -> [[Color]]
render board = partition
  width
  [ colorAt board (x, y)
  | y <- reverse [miny .. maxy]
  , x <- reverse [minx .. maxx]
  ]
 where
  list  = Map.toList board
  minx  = minimum $ map (fst . fst) list
  maxx  = maximum $ map (fst . fst) list
  miny  = minimum $ map (snd . fst) list
  maxy  = maximum $ map (snd . fst) list
  width = maxx - minx + 1
