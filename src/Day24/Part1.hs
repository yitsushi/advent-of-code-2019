module Day24.Part1
  ( solve
  )
where

import           Control.Monad.State.Lazy
import qualified Data.Set                      as Set
import           Day24.Lib

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input      = show $ dup seq
 where
  seq =
    iterate (execState lifeGoesOn) Eris { surface = parseSurface $ lines input }

dup :: [Eris] -> Maybe Int
dup xs = dup' xs Set.empty
 where
  dup' [] _ = Nothing
  dup' (x : xs) s | Set.member bio s = Just bio
                  | otherwise        = dup' xs (Set.insert bio s)
    where bio = biodiversity x
