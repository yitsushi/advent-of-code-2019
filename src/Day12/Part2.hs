module Day12.Part2
  ( solve
  )
where

import           Day12.Lib
import           Lib

axisFilter :: ([(Int, Int)], Char) -> [Moon] -> Bool
axisFilter (initValue, axis) moons = initValue /= state
  where state = allSingleAxisValus axis moons

matchesAxisAfter :: ([(Int, Int)], Char) -> [[Moon]] -> Int
matchesAxisAfter state = (+) 1 . length . takeWhile (axisFilter state) . drop 1

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = show $ foldl lcm 1 matches
 where
  moons    = (map (newMoon . (\x -> read x :: Vector)) . lines) input
  states   = execute moons
  initials = map (\x -> (allSingleAxisValus x moons, x)) "xyz"
  matches  = map (`matchesAxisAfter` states) initials
