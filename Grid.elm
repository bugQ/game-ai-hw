module Grid where

import Graphics.Collage exposing (..)
import Array exposing (Array)
import ArrayToList
import Color exposing (lightCharcoal, lightBrown, blue, darkCharcoal)
import Vec2 exposing (..)
import Random exposing (Generator, Seed)
import Shuffle exposing (shuffle)

type GridNode = Road | Sand | Water | Obstacle
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
get p grid = Array.get (index p grid) grid.array |> Maybe.withDefault Obstacle
set : Point -> GridNode -> Grid -> Grid
set p node grid = { grid | array = Array.set (index p grid) node grid.array }

rand : Grid -> Generator Point
rand grid = let gridH = Array.length grid.array // grid.width in
  Random.pair (Random.int 0 (grid.width - 1)) (Random.int 0 (gridH - 1))

randomize : Grid -> Seed -> (Grid, Seed)
randomize grid seed = let
  checkerboard = Array.initialize (Array.length grid.array) (\i -> case i % 4 of
    1 -> Water
    2 -> Sand
    3 -> Road
    _ -> Obstacle)
  (shuffleboard, seed1) = shuffle seed checkerboard
 in ({ grid | array = shuffleboard }, seed1)

newRand : Int -> Int -> Float -> Seed -> (Grid, Seed)
newRand w h spacing seed = randomize (repeat w h spacing Water) seed

cost node = case node of
  Road -> 1
  Sand -> 2
  Water -> 3
  Obstacle -> 1/0

-- returns points on grid adjacent to given point with movement costs
neighbors : Point -> Grid -> List (Point, Float)
neighbors (x, y) grid = let sqrt2 = sqrt 2 in
  List.filterMap (\(p, c) -> if not (inGrid p grid) then Nothing
     else case get p grid of
      Obstacle -> Nothing
      node -> Just (p, c * cost node))
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
      Road -> lightCharcoal
      Sand -> lightBrown
      Water -> blue
      Obstacle -> darkCharcoal) (square (grid.spacing * 0.8)) |>
    move (gridIndexToScreen i grid)
  ) grid.array
