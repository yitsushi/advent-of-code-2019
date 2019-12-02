module Day01.Part2
  ( solve
  ) where

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

extraFuel' :: Int -> [Int]
extraFuel' mass
  | mass <= 0 = [0]
  | otherwise = extraFuel' (fuel mass) ++ [mass]

withExtraFuel :: Int -> Int
withExtraFuel = sum . init . extraFuel'

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (show . sum . map (withExtraFuel . read) . words) input
