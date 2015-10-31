module TreasureGame where

import Grid

gridW = 20
gridH = 20
spacing = 30



initSim seed0 = let
  (startingGrid, seed1) = Grid.randGrid gridW gridH spacing seed0
  indices = Array.initialize (Array.length grid.array) identity
  (randIndices, seed2) = shuffle seed1 indices
  openIndices = Array.filter
    (\i -> Array.get i grid.array /= Just Obstacle) randIndices
  key1 :: key2 :: key3 :: door1 :: door2 :: door3 :: treasure :: [] =
    Array.toList openIndices |> List.take 7 |> List.map ((flip Grid.deindex) grid)
in
  {}
