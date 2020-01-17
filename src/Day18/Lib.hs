{-# LANGUAGE RecordWildCards #-}

module Day18.Lib where

import           Data.Char
import qualified Data.List          as L
import qualified Data.Map.Strict    as M
import           Data.Maybe
import           Data.Monoid
import           Data.Point
import qualified Data.PriorityQueue as PQ
import qualified Data.Set           as Set
import qualified Data.WalkableMap   as WM
import           Debug.Trace

debugger :: GPS
debugger =
  newGPS
    "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################"

newGPS :: String -> GPS
newGPS input = do
  let c' = parseCave input
  let gps =
        GPS
          { cave = c'
          , collectedKeys = []
          , position = entrance c'
          , cachedRoutes = M.fromList []
          }
  buildCache gps

data Tile
  = Empty
  | Wall
  | Entrance
  | Key Char
  | Door Char
  deriving (Show, Eq)

type Cave = WM.WalkableMap Tile

readTile :: Char -> Tile
readTile '.' = Empty
readTile '#' = Wall
readTile '@' = Entrance
readTile ch
  | isLower ch = Key ch
  | isUpper ch = Door (toLower ch)
  | otherwise = error ("Invalid character: " ++ show ch)

showTile :: Tile -> Char
showTile Empty    = '.'
showTile Wall     = '#'
showTile Entrance = '@'
showTile (Key a)  = a
showTile (Door a) = toUpper a

data GPS =
  GPS
    { cave          :: Cave
    , collectedKeys :: String
    , position      :: Point
    , cachedRoutes  :: M.Map (Point, Point) [Point]
    }

parseCave :: String -> Cave
parseCave =
  snd .
  foldl
    (\(pos@(x, y), cave) ch ->
       if ch == '\n'
         then ((0, y + 1), cave)
         else ((x + 1, y), WM.update pos (readTile ch) cave))
    ((0, 0), WM.singleton Empty)

drawCave :: Cave -> String
drawCave = unlines . flip WM.draw showTile

entrance :: Cave -> Point
entrance = head . WM.findValue Entrance

keys :: Cave -> [(Point, Char)]
keys = map (\(k, Key v) -> (k, v)) . M.toList . M.filter match . WM.content
  where
    match t =
      case t of
        Key a -> True
        _     -> False

planPedometer :: (GPS, Int) -> (GPS, Int)
planPedometer (gps@GPS {..}, steps) = shortest options
  where
    options = map mapper $ remainingRoutes gps
    shortest []  = (gps, steps)
    shortest opt = L.minimumBy (\(_, c1) (_, c2) -> compare c1 c2) opt
    mapper [] = (gps, steps)
    mapper path =
      planPedometer
        ( gps
            { position = head path
            , collectedKeys =
                Set.toList $
                Set.fromList (collectedKeys ++ catMaybes keysOnPath)
            }
        , steps + length path)
      where
        collected =
          Set.toList (Set.fromList (collectedKeys ++ catMaybes keysOnPath))
        keysOnPath = map maybeKey path
        maybeKey pos =
          case WM.valueAt cave pos of
            Key a -> Just a
            _     -> Nothing

routeInCache :: Point -> Point -> GPS -> Maybe [Point]
routeInCache from to GPS {cachedRoutes = cr} =
  getFirst (First normal <> First rev)
  where
    normal = M.lookup (from, to) cr
    rev = M.lookup (to, from) cr

hasKeyFor :: Tile -> GPS -> Bool
hasKeyFor (Door c) gps = c `elem` collectedKeys gps
hasKeyFor Wall _       = False
hasKeyFor _ _          = True

remainingRoutes :: GPS -> [[Point]]
remainingRoutes gps = allRoutes remainingKeys
  where
    remainingKeys =
      [(p, c) | (p, c) <- keys (cave gps), c `notElem` collectedKeys gps]
    isRouteWithoutObstacle :: [Point] -> Bool
    isRouteWithoutObstacle = all isOpen
      where
        isOpen p = hasKeyFor (WM.valueAt (cave gps) p) gps
    allRoutes :: [(Point, Char)] -> [[Point]]
    allRoutes keyList =
      filter isRouteWithoutObstacle $
      catMaybes [routeInCache (position gps) p gps | (p, c) <- keyList]

buildCache :: GPS -> GPS
buildCache gps = gps {cachedRoutes = cache}
  where
    allKeys = keys (cave gps) ++ [(position gps, '@')]
    allPairs = [(x, y) | (x:ys) <- L.tails allKeys, y <- ys]
    cache = foldl build (cachedRoutes gps) allPairs
      where
        build c ((from, _), (to, _)) =
          M.insert (from, to) (path $ route from to) c
          where
            path x =
              case x of
                Nothing                  -> []
                Just PQ.Item {extra = l} -> l
    cave' = cave gps
    route from to =
      WM.pathTo
        PQ.Item {PQ.location = from, PQ.score = 0, PQ.extra = [from]}
        to
        cave' {WM.obstacles = [Wall]}
