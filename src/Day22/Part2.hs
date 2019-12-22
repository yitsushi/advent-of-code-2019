module Day22.Part2
  ( solve
  ) where

import Data.List
import Day22.Lib

{-
deckSize :: Int
deckSize = 119315717514047

shuffleTrick :: Int -> Instruction -> Int
shuffleTrick index Reverse = deckSize - index
shuffleTrick index (Cut n)
  | n > 0 && n < index = index - n
  | n > 0 && n >= index = n - index
  | n < 0 && deckSize + n > index = abs n - (deckSize - index)
  | n < 0 && deckSize + n < index = index + abs n
  | otherwise = error "what the fuck"
shuffleTrick index (Increment n) = index * n `mod` deckSize

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input =
  show $ foldl shuffleTrick 2020 (concat $ replicate times instructions)
  where
    instructions = (map parseLine . lines) input
    times = 101741582076661
-}
solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = "Not Implemented Yet!"
