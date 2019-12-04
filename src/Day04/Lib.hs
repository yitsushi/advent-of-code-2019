module Day04.Lib
  ( toBound
  , validPassword
  , allPossibleCode
  , hasDouble
  ) where

import Data.List

newtype Bound =
  Bound (Int, Int)
  deriving (Show)

toBound :: [Int] -> Bound
toBound (x:y:_) = Bound (x, y)

allPossibleCode :: Bound -> [Int]
allPossibleCode (Bound (x, y)) = [x .. y]

validPassword :: Int -> Bool
validPassword code
  | length (show code) /= 6 = False
  | otherwise = check digits && length (nub digits) /= length digits
  where
    digits = show code
    check :: String -> Bool
    check [] = True
    check [a] = True
    check (a:b:xs)
      | a <= b = check (b : xs)
      | otherwise = False

hasDouble :: Eq a => [a] -> Bool
hasDouble = elem 2 . map length . group
