module Lib where

import Data.List

data Vector
  = IntVector2D (Int, Int)
  | IntVector3D (Int, Int, Int)
  deriving (Eq, Ord, Show)

-- Manhattan distance for a given vector
manhattan :: Vector -> Int
manhattan (IntVector2D (x, y)) = abs x + abs y
manhattan (IntVector3D (x, y, z)) = abs x + abs y + abs z

-- lpad <padding character> -> <desired length> -> <string to ;pad>
lpad :: Char -> Int -> String -> String
lpad with len xs
  | len < length xs = xs
  | otherwise = replicate (len - length ys) with ++ ys
  where
    ys = take len xs

-- splitBy <delimiter>
splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x:xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
