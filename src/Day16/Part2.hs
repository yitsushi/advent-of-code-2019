module Day16.Part2
  ( solve
  )
where

import           Day16.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = process' input
 where
  offset = read $ take 7 input
  list   = concat . replicate 10000 . parse
  process' =
    concatMap show
      . take 8
      . last
      . take 101
      . iterate process
      . drop offset
      . list
  -- shadow the original process because with larg enough offset,
  -- the pattern will be list that contains only
  -- digit 0 and then a lot of digit 1
  -- so we can drop the first 'offset' digits
  process = map oneDigit . scanr1 (+)
