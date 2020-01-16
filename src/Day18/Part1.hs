module Day18.Part1
  ( solve
  ) where

import qualified Data.Map.Strict as M
import           Day18.Lib
import           Debug.Trace

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show steps
  where
    cave' = parseCave input
    start = entrance cave'
    (gps, steps) =
      planPedometer
        ( GPS
            { cave = cave'
            , collectedKeys = []
            , position = start
            , cachedRoutes = M.fromList []
            }
        , 0)
