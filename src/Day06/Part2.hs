module Day06.Part2
  ( solve
  ) where

import Day06.Lib

-- It's a bit tricky, I'll remove the branchpoint,
-- but I don't remove the initial point from the results
-- so the sum adds up (-1) (+1).
-- Easier than take care of them, and you know GTD!!!
removeShared :: [NodeName] -> [NodeName] -> [[NodeName]]
removeShared (x:xs) (y:ys)
  | x == y = removeShared xs ys
  | otherwise = [x : xs, y : ys]

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show $ sum $ map length $ removeShared you san
  where
    connections = (map read . words) input
    root = (head . nodesWithoutParent) connections
    tree = buildTreeFrom connections root
    [you, san] = map (init . buildPathTo tree) ["YOU", "SAN"]
