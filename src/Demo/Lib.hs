module Demo.Lib where

parseInt :: String -> Int
parseInt str
  | head str == '+' = read $ tail str
  | otherwise = read str
