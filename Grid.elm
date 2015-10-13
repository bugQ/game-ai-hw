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

toVec2 (x, y) = (toFloat x, toFloat y)

size grid = grid.width * Array.length grid.array
repeat w h node = { array = Array.repeat (w * h) node, width = w }
index (x, y) grid = y * grid.width + x
deindex i grid = (i % grid.width, i // grid.width)
get p grid = Array.get (index p grid) grid.array |> Maybe.withDefault Untraversable
set p grid = { grid | array <- Array.set (index p grid) Untraversable grid.array }
unset p grid = { grid | array <- Array.set (index p grid) Traversable grid.array }
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
