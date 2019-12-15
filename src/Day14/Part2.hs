module Day14.Part2
  ( solve
  ) where

import Day14.Lib
import Day14.Pocket
import Day14.ResourceDatabase

ceilInt :: Int -> Int -> Int
ceilInt a b = ceiling (fromIntegral a / fromIntegral b)

-- steps could be more then one, because one of the examples
-- requires more ORE / FUEL therefor we can get FUEL
-- from my inpu, so we could start with 460664,
-- but the difference is like 0.01s
-- if I raise initial steps to 460664
-- and like 0.03s if I set the starting fuel value to 460664.
--
-- tl;dr: I don't care
solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = show $ approx 1 1
  where
    target = 10 ^ 12
    database =
      (foldl saveResource newResourceDatabase . map parseRecipe . lines) input
    approx current step
      | step == 1 && requiredNext >= target = current
      | required < target && requiredNext < target =
        approx (current + step) (step * 2)
      | otherwise = approx current (ceilInt step 2)
      where
        required = oreForFuel database current
        requiredNext = oreForFuel database (current + step)
