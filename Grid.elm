module Grid where

import Graphics.Collage exposing (..)
import Array exposing (Array)
import ArrayToList
import Color exposing (lightBlue, darkCharcoal)
import Vec2 exposing (..)

type GridNode = Traversable | Untraversable
type alias Point = (Int, Int)

type alias Grid = {
  array : Array GridNode,
  width : Int
}

toVec2 : Point -> Vec2
toVec2 (x, y) = (toFloat x, toFloat y)

size : Grid -> Int
size grid = grid.width * Array.length grid.array
repeat : Int -> Int -> GridNode -> Grid
repeat w h node = { array = Array.repeat (w * h) node, width = w }
index : Point -> Grid -> Int
index (x, y) grid = y * grid.width + x
deindex : Int -> Grid -> Point
deindex i grid = (i % grid.width, i // grid.width)
get : Point -> Grid -> GridNode
get p grid = Array.get (index p grid) grid.array |> Maybe.withDefault Untraversable
set : Point -> Grid -> Grid  -- sets Untraversable (adds obstacle)
set p grid = { grid |
  array <- Array.set (index p grid) Untraversable grid.array }
unset : Point -> Grid -> Grid  -- sets Traversable (removes obstacle)
unset p grid = { grid |
  array <- Array.set (index p grid) Traversable grid.array }

toggle : Point -> Grid -> Grid
toggle p grid = case (get p grid) of
  Traversable -> set p grid
  Untraversable -> unset p grid

neighbors : Point -> List Point
neighbors (x, y) =
 [ (x-1, y-1)
 , (x, y-1)
 , (x+1, y-1)
 , (x+1, y)
 , (x+1, y+1)
 , (x, y+1)
 , (x-1, y+1)
 , (x-1, y)
 ]

gridPointToScreen : Point -> Grid -> Vec2
gridPointToScreen p grid = let
  spacing = 30
  squareSize = spacing * 0.8
  offset = toFloat grid.width / 2
 in
  ((toVec2 p .-. (offset, offset)) .* spacing)

gridIndexToScreen : Int -> Grid -> Vec2
gridIndexToScreen i grid = gridPointToScreen (deindex i grid) grid

drawGrid : Grid -> List Form
drawGrid grid = ArrayToList.indexedMap (
    \i node -> filled (case node of
      Traversable -> lightBlue
      Untraversable -> darkCharcoal) (square 24) |>
    move (gridIndexToScreen i grid)
  ) grid.array
