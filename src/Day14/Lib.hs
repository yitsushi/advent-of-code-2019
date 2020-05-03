module Day14.Lib where

import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Day14.Pocket
import           Day14.ResourceDatabase
import           Lib

oreForFuel :: ResourceDatabase -> Int -> Int
oreForFuel database qty = required
 where
  (_, required) = itemInPocket "ORE" pocket
  pocket        = requiredOreFor database newPocket "FUEL" qty

requiredOreFor :: ResourceDatabase -> Pocket -> String -> Int -> Pocket
requiredOreFor db pocket name qty = pocket'
 where
  item = fromMaybe (error ("I dont't know this iteam: " ++ name))
    $ Map.lookup name db
  ratio   = ceiling $ fromIntegral qty / fromIntegral (rQuantity item)
  pocket' = foldl walk
                  (addItem name (ratio * rQuantity item) pocket)
                  (rIngredients item)
  walk p ing | riName ing == "ORE" = addItem (riName ing) required p
             | unused < required   = useItem (riName ing) required down
             | otherwise           = useItem (riName ing) required p
   where
    slot@(_, unused) = itemInPocket (riName ing) p
    required         = ratio * riQuantity ing
    down             = requiredOreFor db p (riName ing) (required - unused)

parseRecipe :: String -> Resource
parseRecipe line = resourceFromTuple result ingredients
 where
  separate :: String -> (Int, String)
  separate l = (read qty :: Int, name)
    where [qty, name] = splitOn ' ' $ dropWhile (== ' ') l
  ingredients =
    (map (ingredientFromTuple . separate) . splitOn ',' . init . takeWhile
        (/= '=')
      )
      line
  result = (separate . tail . dropWhile (/= '>')) line
