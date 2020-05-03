module Day08.Part1
  ( solve
  )
where

import           Day08.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = show $ product $ map (`numberOfLayerDigits` target) [1, 2]
 where
  bytes  = (filter (>= 0) . stringToIntList) input
  image  = parseImage (25, 6) bytes
  target = layerWithFewestZeros image
