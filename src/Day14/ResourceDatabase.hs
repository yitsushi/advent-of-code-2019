module Day14.ResourceDatabase where

import qualified Data.Map.Strict as Map

data ResourceIngredient =
  ResourceIngredient
    { riName :: String
    , riQuantity :: Int
    }
  deriving (Show, Eq)

data Resource =
  Resource
    { rName :: String
    , rQuantity :: Int
    , rIngredients :: [ResourceIngredient]
    }
  deriving (Show, Eq)

type ResourceDatabase = Map.Map String Resource

newResourceDatabase :: ResourceDatabase
newResourceDatabase = Map.empty :: ResourceDatabase

saveResource :: ResourceDatabase -> Resource -> ResourceDatabase
saveResource db res = Map.insert (rName res) res db

whatCanIDoFrom :: ResourceDatabase -> String -> ResourceDatabase
whatCanIDoFrom db name = Map.filter hasAsIngredient db
  where
    hasAsIngredient res = any (\x -> name == riName x) (rIngredients res)

ingredientFromTuple :: (Int, String) -> ResourceIngredient
ingredientFromTuple (qty, name) =
  ResourceIngredient {riName = name, riQuantity = qty}

resourceFromTuple :: (Int, String) -> [ResourceIngredient] -> Resource
resourceFromTuple (qty, name) ings =
  Resource {rName = name, rQuantity = qty, rIngredients = ings}
