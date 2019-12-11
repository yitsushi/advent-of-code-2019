module Day08.Lib where

import qualified Data.Char as C
import qualified Data.List as L
import Lib

type Dimension = (Int, Int)

type Layer = [[Int]]

data Image =
  Image Dimension [Layer]
  deriving (Show)

parseImage :: Dimension -> [Int] -> Image
parseImage (width, height) content = Image (width, height) layers
  where
    layers = (partition height . partition width) content

stringToIntList :: String -> [Int]
stringToIntList = map converter
  where
    converter x
      | c >= 0x30 && c <= 0x39 = c - 0x30
      | otherwise = -99
      where
        c = C.ord x

render :: Image -> [String]
render (Image (width, height) layers) =
  map (map c') $ foldl merge pseudoLayer layers
  where
    c' :: Int -> Char
    c' x =
      if x == 1
        then 'X'
        else ' '
    pseudoLayer = replicate height $ replicate width 2
    merge :: Layer -> Layer -> Layer
    merge visible next =
      partition width $ zipWith (curry merge') (concat visible) (concat next)
      where
        merge' :: (Int, Int) -> Int
        merge' (x, y)
          | x == 2 = y
          | otherwise = x

--        map merge' $ zip (concat visible) (concat next)
countItem :: Eq a => a -> [a] -> Int
countItem value = length . filter (== value)

numberOfLayerDigits :: Int -> Layer -> Int
numberOfLayerDigits value = countItem value . concat

layerWithFewestZeros :: Image -> Layer
layerWithFewestZeros (Image _ layers) = L.minimumBy compare' layers
  where
    compare' x y
      | numberOfLayerDigits 0 x < numberOfLayerDigits 0 y = LT
      | otherwise = GT
