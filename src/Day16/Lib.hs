module Day16.Lib where

import Data.Char

parse :: String -> [Int]
parse = map digitToInt . filter isDigit

process :: [Int] -> [Int]
process input = zipWith (curry calc) [1 ..] input
  where
    calc (index, digit) =
      oneDigit $ sum $ zipWith (*) input (currentPattern index)
      where
        currentPattern num = drop 1 $ concatMap (replicate num) base
          where
            base = cycle [0, 1, 0, -1]

oneDigit :: Int -> Int
oneDigit = flip mod 10 . abs
