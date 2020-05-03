module Demo.Part2
  ( solve
  )
where

import qualified Data.Set                      as Set
import           Demo.Lib

solve :: String -> String
solve = show . tracker 0 Set.empty . cycle . map parseInt . words

tracker :: Int -> Set.Set Int -> [Int] -> Int
tracker value visited (x : xs) = next
 where
  result = value + x
  next   = if Set.member result visited
    then result
    else tracker result (Set.insert result visited) xs
