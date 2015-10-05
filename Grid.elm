module Grid where

import Graphics.Collage exposing (..)
import Array exposing (Array)
import Color exposing (lightBlue, darkCharcoal)

type GridNode = Traversable | Untraversable

type alias Grid = {
  array : Array GridNode,
  width : Int
}

type alias Point = {
  x : Int,
  y : Int
}

gridW = 15
gridH = 15

repeat w h node = { array = Array.repeat (w * h) node, width = w }
index (x, y) = y * gridW + x
get p grid = Array.get (index p) grid.array
set p grid = { grid | array <- Array.set (index p) Untraversable grid.array }
unset p grid = { grid | array <- Array.set (index p) Traversable grid.array }
toggle p grid = case (get p grid) of
  Nothing -> grid
  Just Traversable -> set p grid
  Just Untraversable -> unset p grid

drawGrid : Grid -> List Form
drawGrid grid = let
    spacing = 30
    squareSize = spacing * 0.8
    offset = grid.width // 2
  in Array.toList <| Array.indexedMap (
    \i node -> filled (case node of
      Traversable -> lightBlue
      Untraversable -> darkCharcoal) (square squareSize) |>
    move (
      toFloat (i % grid.width - offset) * spacing,
      toFloat (i // grid.width - offset) * spacing)
  ) grid.array
