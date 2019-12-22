module Day22.Lib where

import Data.List as L

data Instruction
  = Reverse
  | Cut Int
  | Increment Int

type Deck = [Int]

parseLine :: String -> Instruction
parseLine line
  | take 3 line == "cut" = Cut (read $ drop 4 line)
  | take 9 line == "deal into" = Reverse
  | take 9 line == "deal with" = Increment (read $ drop 20 line)
  | otherwise = error "What the fuck?"

shuffle :: Deck -> Instruction -> Deck
shuffle deck Reverse = reverse deck
shuffle deck (Cut n)
  | n > 0 = drop n deck ++ take n deck
  | otherwise = drop (l + n) deck ++ take (l + n) deck
  where
    l = length deck
shuffle deck (Increment n) =
  map snd $ L.sortBy order $ zipWith (curry placement) [0 ..] deck
  where
    l = length deck
    placement (position, value) = (position * n `mod` l, value)
    order x y = compare (fst x) (fst y)
