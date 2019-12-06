module Day06.Part1
  ( solve
  ) where

import Data.List
import Day06.Lib

countdown :: Int -> Node -> Int
countdown level (Node _ []) = level
countdown level (Node _ ch) = level + (sum . map (countdown (level + 1))) ch

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show (countdown 0 tree)
  where
    connections = (map read . words) input
    root = (head . nodesWithoutParent) connections
    tree = buildTreeFrom connections root
