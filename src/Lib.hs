module Lib where

-- lpad <padding character> -> <desired length> -> <string to ;pad>
lpad :: Char -> Int -> String -> String
lpad with len xs = replicate (len - length ys) with ++ ys
  where
    ys = take len xs
