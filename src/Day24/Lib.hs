module Day24.Lib where

import Control.Monad.State.Lazy
import Lib

readSpot :: Char -> Int
readSpot '.' = 0
readSpot '#' = 1

type Surface = [(Int, Int)]

newtype Eris =
  Eris
    { surface :: Surface
    }

type ErisState = State Eris

instance Show Eris where
  show = show . biodiversity

lifeGoesOn :: ErisState Int
lifeGoesOn = do
  s <- gets surface
  let s' = map (calc . neighbors) s
      calc (i, v, n)
        | v == 1 && n /= 1 = (i, 0)
        | v == 0 && n > 0 && n < 3 = (i, 1)
        | otherwise = (i, v)
      neighbors (i, v) = (i, v, sum (positive ++ negative))
        where
          positive =
            [ snd $ s !! (i + t)
            | t <- [1, 5]
            , i + t >= 0
            , i + t < length s
            , (i + t) `mod` 5 >= (i `mod` 5)
            ]
          negative =
            [ snd $ s !! (i + t)
            | t <- [-1, -5]
            , i + t >= 0
            , i + t < length s
            , (i + t) `mod` 5 <= (i `mod` 5)
            ]
      newEris = Eris {surface = s'}
  put newEris
  return $ biodiversity newEris

biodiversity :: Eris -> Int
biodiversity = sum . map calc . surface
  where
    calc (i, v) = v * 2 ^ i

draw :: Eris -> String
draw = unlines . partition 5 . map convert . surface
  where
    convert (_, x)
      | x == 1 = '#'
      | otherwise = '.'

parseSurface :: [String] -> Surface
parseSurface = zip [0 ..] . concatMap (map readSpot)
