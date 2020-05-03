module Day10.Lib where

import           Data.List

type MapPoint a = ((Int, Int), a)

type MapValue = Int

parse :: [String] -> [MapPoint MapValue]
parse lines =
  (map unwrapMaybe . filter filterCondition . concatMap buildMaybeMap . zip
      [0 .. length lines]
    )
    lines
 where
  buildMaybeMap :: (Int, String) -> [MapPoint (Maybe Int)]
  buildMaybeMap (y, l) = map (buildMapPoint y) (zip [0 .. length l] l)
  buildMapPoint :: Int -> (Int, Char) -> MapPoint (Maybe Int)
  buildMapPoint y (x, v) | v == '#'  = ((x, y), Just 0)
                         | otherwise = ((x, y), Nothing)
  filterCondition :: MapPoint (Maybe Int) -> Bool
  filterCondition (_, x) = case x of
    Just _  -> True
    Nothing -> False
  unwrapMaybe :: MapPoint (Maybe Int) -> MapPoint Int
  unwrapMaybe ((x, y), jv) = case jv of
    Just v -> ((x, y), v)
    _      -> error "Nothing went horribly wrong!"

calculateVisibility :: [MapPoint MapValue] -> [MapPoint MapValue]
calculateVisibility m = (simplify . concatMap checker) allPairs
 where
  allPairs = [ (x, y) | (x : ys) <- tails m, y <- ys ]
  checker :: (MapPoint MapValue, MapPoint MapValue) -> [MapPoint Bool]
  checker (p1@((x1, y1), _), p2@((x2, y2), _)) = [pn1, pn2]
   where
    result = blockedView p1 p2 m
    pn1    = ((x1, y1), result)
    pn2    = ((x2, y2), result)
  simplify :: [MapPoint Bool] -> [MapPoint MapValue]
  simplify =
    map (repack . unzip)
      . filter (not . null)
      . map (filter (\(_, v) -> not v))
      . group
      . sort
   where
    repack :: ([(Int, Int)], [Bool]) -> MapPoint MapValue
    repack (coord : _, values) = (coord, length values)

visibleFromWithAngle
  :: MapPoint MapValue -> [MapPoint MapValue] -> [MapPoint Float]
visibleFromWithAngle station others =
  map unwrapMaybe $ filter filterCondition $ map (checker station) others
 where
  checker :: MapPoint MapValue -> MapPoint MapValue -> Maybe (MapPoint Float)
  checker s@((x1, y1), _) next@((x2, y2), _)
    | x1 == x2 && y1 == y2 = Nothing
    | result               = Nothing
    | not result           = Just ((x2, y2), angle * 360 / (2 * pi))
   where
    result = blockedView s next others
    angle | an' >= 0  = an'
          | otherwise = 2 * pi + an'
     where
      an' = atan2 dy dx - pi / 2
      dx  = fromIntegral (x1 - x2)
      dy  = fromIntegral (y1 - y2)
  filterCondition :: Maybe (MapPoint Float) -> Bool
  filterCondition p = case p of
    Just _  -> True
    Nothing -> False
  unwrapMaybe :: Maybe (MapPoint Float) -> MapPoint Float
  unwrapMaybe jv = case jv of
    Just v -> v
    _      -> error "Nothing went horribly wrong!"

blockedView :: MapPoint Int -> MapPoint Int -> [MapPoint Int] -> Bool
blockedView _  _  []       = False
blockedView p1 p2 (x : xs) = (between' && onLine') || blockedView p1 p2 xs
 where
  between' = isBetween p1 p2 x
  onLine'  = isOnLine p1 p2 x

isBetween :: MapPoint a -> MapPoint a -> MapPoint a -> Bool
isBetween ((x1, y1), _) ((x2, y2), _) ((xt, yt), _)
  | (x1, y1) == (xt, yt) = False
  | (x2, y2) == (xt, yt) = False
  | x1 == x2 && x1 == xt = betweenY
  | y1 == y2 && y1 == yt = betweenX
  | otherwise            = betweenX && betweenY
 where
  (maxX, minX) = (maximum [x1, x2], minimum [x1, x2])
  (maxY, minY) = (maximum [y1, y2], minimum [y1, y2])
  betweenX     = maxX > xt && minX < xt
  betweenY     = maxY > yt && minY < yt

isOnLine :: MapPoint a -> MapPoint a -> MapPoint a -> Bool
isOnLine ((x1, y1), _) ((x2, y2), _) ((xt, yt), _)
  | x1 == x2  = xt == x1
  | y1 == y2  = yt == y1
  | otherwise = (slope * fromIntegral (xt - x1)) == fromIntegral (yt - y1)
  where slope = fromIntegral (y1 - y2) / fromIntegral (x1 - x2)
