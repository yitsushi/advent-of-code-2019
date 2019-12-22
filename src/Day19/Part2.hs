{-# LANGUAGE RecordWildCards #-}

module Day19.Part2
  ( solve
  ) where

import Data.Map.Strict as Map
import Data.Maybe
import Day19.Lib
import Debug.Trace
import Intcode

target :: Int
target = 100

controller :: BeamDetector -> Maybe (Int, Int)
controller detector@BeamDetector {..}
  | trace (show (beamStartAt, lastSector, lx - beamX)) False = undefined
  | isJust beamStartAt &&
      lastSector == Beam && historicalX == Beam && historicalY == Beam = Nothing
  | isJust beamStartAt &&
      lx > beamX && lastSector /= Beam && sectorBefore == Beam =
    Just (beamX, ly + 1)
  | otherwise = Just (nextX, nextY)
  where
    (lx, ly) = fromMaybe (-1, -1) bdLastInput
    (beamX, beamY) = fromMaybe (-1, -1) beamStartAt
    lastSector = sectorAt detector (lx, ly)
    sectorBefore = sectorAt detector (lx - 1, ly)
    historicalX = sectorAt detector (lx - target, ly)
    historicalY = sectorAt detector (lx, ly - target)
    nextX
      | lx < beamX = beamX
      | ly > 10 = lx + 1
      | otherwise = (lx + 1) `mod` 20
    nextY =
      if lx == 0
        then ly + 1
        else ly

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input
  | trace (unlines $ drawSpace detector) False = undefined
  | trace (show $ bdLastInput detector) False = undefined
  | otherwise = (show . length . Map.filter (== Beam)) (bdSpace detector)
  where
    tape = parse input
    detector = executeBeamDetector $ newBeamDetector tape controller
