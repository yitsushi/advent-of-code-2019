{-# LANGUAGE RecordWildCards #-}

module Day19.Part1
  ( solve
  ) where

import Data.Map.Strict as Map
import Data.Maybe
import Day19.Lib
import IntcodeMachine

controller :: BeamDetector -> Maybe (Int, Int)
controller detector@BeamDetector {..}
  | nextY > 49 = Nothing
  | isJust beamStartAt &&
      lx > beamX && lastSector /= Beam && sectorBefore == Beam =
    Just (beamX, ly + 1)
  | otherwise = Just (nextX, nextY)
  where
    (lx, ly) = fromMaybe (-1, -1) bdLastInput
    (beamX, beamY) = fromMaybe (-1, -1) beamStartAt
    lastSector = sectorAt detector (lx, ly)
    sectorBefore = sectorAt detector (lx - 1, ly)
    nextX =
      if lx < beamX
        then beamX
        else (lx + 1) `mod` 50
    nextY =
      if lx == 0
        then ly + 1
        else ly

solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = (show . length . Map.filter (== Beam)) (bdSpace detector)
  where
    tape = parse input
    detector = executeBeamDetector $ newBeamDetector tape controller
