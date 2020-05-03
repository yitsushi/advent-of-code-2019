module Day14.Pocket where

import qualified Data.Map.Strict               as Map
import           Data.Maybe

type ItemSlot = (Int, Int)

type Pocket = Map.Map String ItemSlot

newPocket :: Pocket
newPocket = Map.empty :: Pocket

itemInPocket :: String -> Pocket -> ItemSlot
itemInPocket name = fromMaybe (0, 0) . Map.lookup name

addItem :: String -> Int -> Pocket -> Pocket
addItem name qty pocket = Map.insert name (used, unused + qty) pocket
  where (used, unused) = itemInPocket name pocket

useItem :: String -> Int -> Pocket -> Pocket
useItem name qty pocket
  | unused < qty = error
    ("You don't have enough " ++ name ++ ": " ++ show state)
  | otherwise = Map.insert name (used + qty, unused - qty) pocket
  where state@(used, unused) = itemInPocket name pocket
