module Day02.Lib where

import Lib (splitOn)

parse :: String -> [Int]
parse = map read . splitOn ','

getOps :: Int -> ([Int] -> [Int])
getOps from = take 4 . drop from

process :: Int -> [Int] -> [Int] -> [Int]
process _ (99:_) tape = tape
process head' (op:src2:src1:dest:_) tape
  | op == 1 = process next (getOps next tapeAdd') tapeAdd'
  | op == 2 = process next (getOps next tapeMul') tapeMul'
  | otherwise = error "Invalid operation!"
  where
    p1 = tape !! src1
    p2 = tape !! src2
    before = take dest tape
    after = drop (dest + 1) tape
    tapeAdd' = before ++ [p1 + p2] ++ after
    tapeMul' = before ++ [p1 * p2] ++ after
    next = head' + 4
process h ops tape =
  error
    ("Something went horribly wrong!" ++
     show h ++ " :: " ++ show ops ++ " -> " ++ show tape)
