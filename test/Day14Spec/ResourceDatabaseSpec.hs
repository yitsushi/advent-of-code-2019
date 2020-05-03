module Day14Spec.ResourceDatabaseSpec where

import qualified Data.Map.Strict               as Map
import           Day14.ResourceDatabase
import           Day14Spec.Helper
import           Test.Hspec

spec :: Spec
spec = describe "parseRecipe" $ do
  describe "example1" $ do
    database <- runIO (loadDatabase "example1")
    it "has FUEL" $ Map.lookup "FUEL" database `shouldBe` Just
      (Resource
        { rName        = "FUEL"
        , rQuantity    = 1
        , rIngredients = [ ResourceIngredient { riName = "A", riQuantity = 7 }
                         , ResourceIngredient { riName = "E", riQuantity = 1 }
                         ]
        }
      )
    it "has A" $ Map.lookup "A" database `shouldBe` Just
      (Resource
        { rName        = "A"
        , rQuantity    = 10
        , rIngredients = [ ResourceIngredient { riName     = "ORE"
                                              , riQuantity = 10
                                              }
                         ]
        }
      )
  describe "example2" $ do
    database <- runIO (loadDatabase "example2")
    it "has FUEL" $ Map.lookup "FUEL" database `shouldBe` Just
      (Resource
        { rName        = "FUEL"
        , rQuantity    = 1
        , rIngredients = [ ResourceIngredient { riName = "AB", riQuantity = 2 }
                         , ResourceIngredient { riName = "BC", riQuantity = 3 }
                         , ResourceIngredient { riName = "CA", riQuantity = 4 }
                         ]
        }
      )
    it "has A" $ Map.lookup "A" database `shouldBe` Just
      (Resource
        { rName        = "A"
        , rQuantity    = 2
        , rIngredients = [ResourceIngredient { riName = "ORE", riQuantity = 9 }]
        }
      )
  describe "ingredientFromTuple" $ do
    let ingredient = ingredientFromTuple (8, "ASD")
    it "Name" $ riName ingredient `shouldBe` "ASD"
    it "Quantity" $ riQuantity ingredient `shouldBe` 8
  describe "resourceFromTuple" $ do
    let ingredients =
          [ingredientFromTuple (13, "ASD"), ingredientFromTuple (8, "DSA")]
    let resource = resourceFromTuple (2, "FUEL") ingredients
    it "Name" $ rName resource `shouldBe` "FUEL"
    it "Quantity" $ rQuantity resource `shouldBe` 2
    it "Ingredients" $ rIngredients resource `shouldBe` ingredients
