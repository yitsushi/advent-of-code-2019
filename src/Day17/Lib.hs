{-# LANGUAGE RecordWildCards #-}

module Day17.Lib where

import           Data.Char
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import           IntcodeMachine
import           Lib

data Direction
  = North
  | East
  | South
  | West
  | Invalid
  deriving (Show, Eq)

data Tile
  = Empty
  | Scaffold
  | Robot Direction
  | TextValue Char
  deriving (Show, Eq)

tileChar :: Tile -> Char
tileChar Empty         = '.'
tileChar Scaffold      = '#'
tileChar (TextValue v) = v
tileChar (Robot     a) = case a of
  North   -> '^'
  East    -> '>'
  South   -> 'v'
  West    -> '<'
  Invalid -> 'X'

readTile :: Int -> Tile
readTile 46  = Empty
readTile 35  = Scaffold
readTile 94  = Robot North
readTile 62  = Robot East
readTile 118 = Robot South
readTile 60  = Robot West
readTile 88  = Robot Invalid
readTile c   = TextValue (chr c)
type Screen = Map.Map (Int, Int) Tile

data Machine =
  Machine
    { machineProgram :: Computer
    , machineScreen  :: Screen
    }

parseScreen :: [Int] -> Screen
parseScreen stream = screen
 where
  (_, _, screen) = foldl
    (\(x, y, screen) ch -> if ch == 10
      then (0, y + 1, screen)
      else (x + 1, y, Map.insert (x, y) (readTile ch) screen)
    )
    (0, 0, Map.fromList [] :: Screen)
    stream

tileAt :: Screen -> (Int, Int) -> Tile
tileAt screen position = Maybe.fromMaybe Empty $ Map.lookup position screen

renderScreen :: Machine -> [String]
renderScreen Machine { machineScreen = screen } =
  map (map tileChar) $ partition
    width
    [ tileAt screen (x, y) | y <- [miny .. maxy], x <- [minx .. maxx] ]
 where
  list  = Map.toList screen
  minx  = minimum $ map (fst . fst) list
  maxx  = maximum $ map (fst . fst) list
  miny  = minimum $ map (snd . fst) list
  maxy  = maximum $ map (snd . fst) list
  width = maxx - minx + 1

executeMachine :: Machine -> Machine
executeMachine machine@Machine {..} = machine { machineProgram = code
                                              , machineScreen  = screen code
                                              }
 where
  code   = boot machineProgram
  screen = parseScreen . output

findIntersections :: Screen -> [(Int, Int)]
findIntersections screen = Map.keys $ Map.filterWithKey isIntersection screen
 where
  isIntersection (x, y) v | v /= Scaffold = False
                          | otherwise     = all (== Scaffold) neighbors
   where
    neighbors =
      [ tileAt screen (x + 1, y)
      , tileAt screen (x - 1, y)
      , tileAt screen (x, y + 1)
      , tileAt screen (x, y - 1)
      ]

newMachine :: String -> Machine
newMachine tape = Machine { machineProgram = loadComputer tape []
                          , machineScreen  = Map.singleton (0, 0) Empty
                          }
