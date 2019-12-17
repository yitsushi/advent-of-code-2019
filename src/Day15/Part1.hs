module Day15.Part1
  ( solve
  ) where

import Data.Maybe
import Data.Point
import qualified Data.PriorityQueue as PQ
import qualified Data.WalkableMap as WM
import Day15.Lib
import Debug.Trace
import Intcode

controller :: Drone -> Either (Maybe Direction) [Point]
controller drone
   -- | code == Found = Left Nothing -- error "This is the OxygenSystem"
  | not (null (autoPilot drone)) = Right (autoPilot drone)
    -- | trace (unlines (draw drone) ++ "\n\n") False = undefined
  | null nextStep = Left Nothing
  | length nextStep == 1 =
    Left (Just (vectorToDirection (head nextStep <-> pos)))
  | otherwise = Right nextStep
  where
    code = responseCode drone
    area = mapAround drone
    pos = dronePosition drone
    posItem = PQ.Item {PQ.location = pos, PQ.score = 0, PQ.extra = [pos]}
    pathToNext = WM.pathToClosestValue posItem Unknown area
    nextStep =
      case pathToNext of
        Nothing -> []
        Just item -> reverse $ init $ PQ.extra item

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input
  | trace (unlines $ draw drone) False = undefined
  | otherwise = show $ length realPath
  where
    tape = parse input
    drone = executeDrone $ newDrone tape controller
    path =
      WM.pathTo
        PQ.Item {PQ.location = (0, 0), PQ.score = 0, PQ.extra = [(0, 0)]}
        ((head . WM.findValue OxygenSystem . mapAround) drone)
        (mapAround drone) {WM.obstacles = [Wall, Unknown]}
    realPath =
      case path of
        Nothing -> error "Shit happened!"
        Just list -> init (PQ.extra list)
