module Day13.Lib where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import IntcodeMachine
import Lib

data Tile
  = Empty
  | Wall
  | Block
  | HorizontalPaddle
  | Ball
  | Score Int
  deriving (Eq, Show)

type ControllerFunction = (Game -> Int)

tileChar :: Tile -> Char
tileChar Empty = ' '
tileChar Wall = '#'
tileChar Block = '@'
tileChar HorizontalPaddle = '_'
tileChar Ball = 'o'
tileChar (Score v) = 'v'

tile :: Int -> Tile
tile 0 = Empty
tile 1 = Wall
tile 2 = Block
tile 3 = HorizontalPaddle
tile 4 = Ball
tile value = Score value

data Game =
  Game
    { gameProgram :: Computer
    , gameController :: ControllerFunction
    , gameScreen :: Screen
    , gameScore :: Int
    }

type Screen = Map.Map (Int, Int) Tile

parseScreen :: Screen -> [Int] -> Screen
parseScreen screen [] = screen
parseScreen screen (x:y:id:rest) =
  parseScreen (Map.insert (x, y) (tile id) screen) rest

executeGame :: Game -> Game
executeGame game@Game {gameProgram = gp, gameController = controller}
  | isTerminated gp = game
  | otherwise = executeGame game'
  where
    input = controller game
    program = boot $ wipeOutput $ feedInput gp input
    game' = parseGameScreen (game {gameProgram = program})

scoreFilter :: Tile -> Bool
scoreFilter t =
  case t of
    Score v -> True
    _ -> False

parseGameScreen :: Game -> Game
parseGameScreen game@Game {gameProgram = gp, gameScreen = gs} = game'
  where
    screen = parseScreen gs (output gp)
    pureScreen = Map.filter (not . scoreFilter) screen
    game' =
      game
        { gameScreen = pureScreen
        , gameScore =
            (score . map snd . Map.toList . Map.filter scoreFilter) screen
        }
      where
        score [] = gameScore game
        score [Score v] = v

getBallPosition :: Game -> (Int, Int)
getBallPosition = head . Map.keys . Map.filter (== Ball) . gameScreen

getPlayerPosition :: Game -> (Int, Int)
getPlayerPosition =
  head . Map.keys . Map.filter (== HorizontalPaddle) . gameScreen

numberOfRemainingBlocks :: Game -> Int
numberOfRemainingBlocks = Map.size . Map.filter (== Block) . gameScreen

tileAt :: Game -> (Int, Int) -> Tile
tileAt game position =
  Maybe.fromMaybe Empty $ Map.lookup position (gameScreen game)

displayGame :: Game -> [String]
displayGame game =
  (:) scoreLine $
  map (map tileChar) $
  partition
    width
    [ tileAt game (x, y)
    | y <- reverse [miny .. maxy]
    , x <- reverse [minx .. maxx]
    ]
  where
    list = Map.toList (gameScreen game)
    minx = minimum $ map (fst . fst) list
    maxx = maximum $ map (fst . fst) list
    miny = minimum $ map (snd . fst) list
    maxy = maximum $ map (snd . fst) list
    width = maxx - minx + 1
    scoreLine' = "Score: " ++ show (gameScore game)
    scoreLine = replicate n ' ' ++ scoreLine' ++ replicate m ' '
      where
        n = (width `div` 2) - (length scoreLine' `div` 2)
        m = width - length scoreLine'

startGame :: Game -> Game
startGame = executeGame

newGame :: Computer -> ControllerFunction -> Game
newGame p c =
  Game
    { gameProgram = p
    , gameController = c
    , gameScreen = Map.singleton (0, 0) Empty
    , gameScore = 0
    }
