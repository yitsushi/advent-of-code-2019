module Day10.Part2
  ( solve
  )
where

import           Data.List
import           Day10.Lib

removeAsteroids :: [MapPoint Int] -> [MapPoint Float] -> [MapPoint Int]
removeAsteroids full sub = map (\x -> (x, 0)) (map fst full \\ map fst sub)

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input =
  show $ (\(x, y) -> x * 100 + y) $ fst $ destroying asteroids !! 199
 where
  asteroids = (parse . words) input
  station   = maximumBy ordering $ calculateVisibility asteroids
  destroying [station]  = []
  destroying asteroids' = visible ++ destroying newList
   where
    visible = sortBy ordering $ visibleFromWithAngle station asteroids'
    newList = removeAsteroids asteroids' visible
  ordering :: Ord a => MapPoint a -> MapPoint a -> Ordering
  ordering (_, connections1) (_, connections2)
    | connections1 < connections2 = LT
    | otherwise                   = GT
