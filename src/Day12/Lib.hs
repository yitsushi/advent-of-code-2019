module Day12.Lib where

import Lib
import Prelude hiding (cycle)

instance Read Vector where
  readsPrec _ input
    | head input /= '<' || last input /= '>' = []
    | length parts /= 3 = []
    | null (selector 'x' parts) = []
    | null (selector 'y' parts) = []
    | null (selector 'z' parts) =
      [(IntVector2D (extraxt 'x' parts, extraxt 'y' parts), "")]
    | otherwise =
      [ ( IntVector3D (extraxt 'x' parts, extraxt 'y' parts, extraxt 'z' parts)
        , "")
      ]
    where
      parts = (map (takeWhile (/= ',')) . words . tail . init) input
      selector key = filter ((==) key . head)
      extraxt key = read . drop 2 . head . selector key

data Moon =
  Moon Vector Vector
  deriving (Eq, Show)

newMoon :: Vector -> Moon
newMoon position = Moon position (IntVector3D (0, 0, 0))

updateVelocity :: Moon -> [Moon] -> Moon
updateVelocity = foldl calculate
  where
    calculate (Moon p1 v1) (Moon p2 v2) = Moon p1 (v1 + normalize (p2 - p1))
      where
        normalize (IntVector3D (x, y, z)) =
          IntVector3D (directionOf x, directionOf y, directionOf z)
          where
            directionOf 0 = 0
            directionOf v = v `div` abs v

move :: Moon -> Moon
move (Moon position velocity) = Moon (position + velocity) velocity

potentialEnergy :: Moon -> Int
potentialEnergy (Moon (IntVector3D (x, y, z)) _) = abs x + abs y + abs z

kineticEnergy :: Moon -> Int
kineticEnergy (Moon _ (IntVector3D (x, y, z))) = abs x + abs y + abs z

totalEnergy :: Moon -> Int
totalEnergy m = potentialEnergy m * kineticEnergy m

allSingleAxisValus :: Char -> [Moon] -> [(Int, Int)]
allSingleAxisValus axis = map (extract axis)
  where
    extract axis' (Moon (IntVector3D (px, py, pz)) (IntVector3D (vx, vy, vz)))
      | axis' == 'x' = (px, vx)
      | axis' == 'y' = (py, vy)
      | axis' == 'z' = (pz, vz)
      | otherwise = error "Axis should be z, y or z"

cycle :: [Moon] -> [Moon]
cycle moons = map (move . update . pairs) moons
  where
    pairs moon = (moon, filter (/= moon) moons)
    update (m, rest) = updateVelocity m rest

execute :: [Moon] -> [[Moon]]
execute moons = moons : execute state
  where
    state = cycle moons
