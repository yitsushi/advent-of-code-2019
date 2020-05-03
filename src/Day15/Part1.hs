module Day15.Part1
  ( solve
  )
where

import           Data.Maybe
import qualified Data.PriorityQueue            as PQ
import qualified Data.WalkableMap              as WM
import           Day15.Lib
import           Debug.Trace

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input | trace (unlines $ draw drone) False = undefined
            | otherwise                          = show $ length realPath
 where
  drone = executeDrone $ newDrone input controller
  path  = WM.pathTo
    PQ.Item { PQ.location = (0, 0), PQ.score = 0, PQ.extra = [(0, 0)] }
    ((head . WM.findValue OxygenSystem . mapAround) drone)
    (mapAround drone) { WM.obstacles = [Wall, Unknown] }
  realPath = case path of
    Nothing   -> error "Shit happened!"
    Just list -> init (PQ.extra list)
