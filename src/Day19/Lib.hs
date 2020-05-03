module Day19.Lib where

import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           IntcodeMachine
import           Lib

data Sector
  = Empty
  | Beam
  | Unknown
  deriving (Show, Eq)

sectorChar :: Sector -> Char
sectorChar Empty   = '.'
sectorChar Beam    = '#'
sectorChar Unknown = ' '

readSector :: Int -> Sector
readSector 0 = Empty
readSector 1 = Beam

type Space = Map.Map (Int, Int) Sector

type Controller = (BeamDetector -> Maybe (Int, Int))

data BeamDetector =
  BeamDetector
    { bdProgram :: Computer
    , bdSpace :: Space
    , bdController :: Controller
    , bdLastInput :: Maybe (Int, Int)
    , originPoint :: Maybe (Int, Int)
    , beamStartAt :: Maybe (Int, Int)
    }

sectorAt :: BeamDetector -> (Int, Int) -> Sector
sectorAt BeamDetector { bdSpace = space } position =
  fromMaybe Unknown $ Map.lookup position space

executeBeamDetector :: BeamDetector -> BeamDetector
executeBeamDetector detector@BeamDetector { bdProgram = computer, bdSpace = space, bdController = controller }
  | isTerminated computer
  = detector
  | isNothing input
  = detector
  | otherwise
  = executeBeamDetector detector' { bdProgram = computer }
 where
  hasChangeInY :: Maybe (Int, Int) -> Maybe (Int, Int) -> Bool
  hasChangeInY (Just (_, iny)) (Just (_, lasty)) = iny /= lasty
  hasChangeInY _               _                 = False
  input = controller detector
  updateInput (Just (x, y)) = (`feedInput` y) . (`feedInput` x)
  computer' = boot $ wipeOutput $ updateInput input computer
  detector_ = if hasChangeInY input (beamStartAt detector)
    then detector { beamStartAt = Nothing }
    else detector
  detector' =
    parseResponse detector_ { bdProgram = computer', bdLastInput = input }

parseResponse :: BeamDetector -> BeamDetector
parseResponse detector@BeamDetector { bdProgram = computer, bdSpace = space, bdLastInput = input, beamStartAt = beamStartAt, originPoint = originPoint }
  = detector { bdSpace = space', beamStartAt = bPoint, originPoint = oPoint }
 where
  value  = readSector $ head $ output computer
  space' = case input of
    Just (x, y) -> Map.insert (x, y) value space
    Nothing     -> space
  oPoint =
    if isNothing originPoint && value == Beam then input else originPoint
  bPoint = if isJust originPoint && isNothing beamStartAt && value == Beam
    then input
    else beamStartAt

drawSpace :: BeamDetector -> [String]
drawSpace detector@BeamDetector { bdSpace = space } =
  map (map sectorChar) $ partition
    width
    [ sectorAt detector (x, y) | y <- [miny .. maxy], x <- [minx .. maxx] ]
 where
  list  = Map.toList space
  minx  = minimum $ map (fst . fst) list
  maxx  = maximum $ map (fst . fst) list
  miny  = minimum $ map (snd . fst) list
  maxy  = maximum $ map (snd . fst) list
  width = maxx - minx + 1

newBeamDetector :: [Int] -> Controller -> BeamDetector
newBeamDetector tape controller = BeamDetector
  { bdProgram    = newComputer tape []
  , bdSpace      = Map.fromList []
  , bdController = controller
  , bdLastInput  = Nothing
  , originPoint  = Nothing
  , beamStartAt  = Nothing
  }
