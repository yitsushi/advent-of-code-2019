module Lib where

--import Data.List
import qualified Data.List as L

data Vector
  = IntVector2D (Int, Int)
  | IntVector3D (Int, Int, Int)
  deriving (Eq, Ord, Show)

-- | [Manhattan distance](https://en.wikipedia.org/wiki/Taxicab_geometry) for a 'Vector'
--
-- >>> manhattan $ IntVector2D (14,-20)
-- 34
--
-- >>> manhattan $ IntVector3D (14,-20,10)
-- 44
manhattan :: Vector -> Int
manhattan (IntVector2D (x, y)) = abs x + abs y
manhattan (IntVector3D (x, y, z)) = abs x + abs y + abs z

-- | Left padding
--
-- >>> lpad '0' 5 "10"
-- "00010"
--
-- >>> lpad 0 5 [1,5]
-- [0,0,0,1,5]
lpad :: a -> Int -> [a] -> [a]
lpad with len xs
  | len < length xs = xs
  | otherwise = replicate (len - length ys) with ++ ys
  where
    ys = take len xs

-- | Split list on a specific element
--
-- >>> splitOn ',' "1,2,3"
-- ["1","2","3"]
--
-- >>> splitOn 1 [9,1,3,1,8,2,5,1,2,5]
-- [[9],[3],[8,2,5],[2,5]]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x:xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)
