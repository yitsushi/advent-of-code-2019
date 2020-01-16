module Day03.Lib
  ( parseWire
  , allPointsOnPath
  ) where

import           Lib

data WireMovement =
  WireMovement Char Int
  deriving (Show)

newtype WirePath =
  WirePath [WireMovement]
  deriving (Show)

allPointsOnPath :: WirePath -> [Vector]
allPointsOnPath (WirePath path) = tail $ foldl calc [IntVector2D (0, 0)] path
  where
    calc history (WireMovement direction count)
      | direction == 'U' = history ++ up'
      | direction == 'D' = history ++ down'
      | direction == 'R' = history ++ right'
      | direction == 'L' = history ++ left'
      | otherwise = error ("Undefined direction: " ++ [direction])
      where
        IntVector2D (fromX, fromY) = last history
        up' = [IntVector2D (fromX + x, fromY) | x <- [1 .. count]]
        down' = [IntVector2D (fromX - x, fromY) | x <- [1 .. count]]
        right' = [IntVector2D (fromX, fromY + y) | y <- [1 .. count]]
        left' = [IntVector2D (fromX, fromY - y) | y <- [1 .. count]]

parseWire :: String -> WirePath
parseWire = WirePath . map decompose . splitOn ','
  where
    decompose s = WireMovement (head s) (read $ tail s)
