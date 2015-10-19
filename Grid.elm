module Grid where

import Graphics.Collage exposing (..)
import Array exposing (Array)
import ArrayToList
import Color exposing (lightBlue, darkCharcoal)
import Vec2 exposing (..)
import Random exposing (Generator)

type GridNode = Traversable | Untraversable
type alias Point = (Int, Int)
type alias Path = List Point

type alias Grid =
 { array : Array GridNode
 , width : Int
 , spacing : Float
 }

toVec2 : Point -> Vec2
toVec2 (x, y) = (toFloat x, toFloat y)

fromVec2 : Vec2 -> Point
fromVec2 (x, y) = (round x, round y)

size : Grid -> Int
size grid = grid.width * Array.length grid.array
repeat : Int -> Int -> Float -> GridNode -> Grid
repeat w h sp node = { array = Array.repeat (w * h) node, width = w, spacing = sp }
inGrid : Point -> Grid -> Bool
inGrid (x, y) grid = x >= 0 && y >= 0
  && x < grid.width && y * grid.width < Array.length grid.array
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

rand : Grid -> Generator Point
rand grid = let gridH = Array.length grid.array // grid.width in
  Random.pair (Random.int 0 (grid.width - 1)) (Random.int 0 (gridH - 1))

-- returns points on grid adjacent to given point with movement costs
neighbors : Point -> Grid -> List (Point, Float)
neighbors (x, y) grid = let sqrt2 = sqrt 2 in
  List.filter (\(p, _) -> inGrid p grid && get p grid /= Untraversable)
    [ ((x-1, y-1), sqrt2)
    , ((x, y-1), 1)
    , ((x+1, y-1), sqrt2)
    , ((x+1, y), 1)
    , ((x+1, y+1), sqrt2)
    , ((x, y+1), 1)
    , ((x-1, y+1), sqrt2)
    , ((x-1, y), 1)
    ]

gridPointToScreen : Point -> Grid -> Vec2
gridPointToScreen p grid = let offset = toFloat (grid.width - 1) / 2 in
  ((toVec2 p .-. (offset, offset)) .* grid.spacing)

gridIndexToScreen : Int -> Grid -> Vec2
gridIndexToScreen i grid = gridPointToScreen (deindex i grid) grid

screenPointToGrid : Vec2 -> Grid -> Point
screenPointToGrid s grid = let offset = toFloat (grid.width - 1) / 2 in
  fromVec2 (s ./ grid.spacing .+. (offset, offset))

drawGrid : Grid -> List Form
drawGrid grid = ArrayToList.indexedMap (
    \i node -> filled (case node of
      Traversable -> lightBlue
      Untraversable -> darkCharcoal) (square (grid.spacing * 0.8)) |>
    move (gridIndexToScreen i grid)
  ) grid.array
