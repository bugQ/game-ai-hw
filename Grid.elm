module Grid exposing (..)

import Collage exposing (..)
import Array exposing (Array)
import ArrayToList
import Color exposing (lightCharcoal, lightBrown, lightBlue, darkCharcoal)
import Vec2 exposing (..)
import Random exposing (Generator, Seed)
import Random.Array

type GridNode = Road | Sand | Water | Obstacle
type alias Point = (Int, Int)
type alias Path = List Point

type alias Grid =
 { array : Array GridNode
 , width : Int
 , spacing : Float
 }

grid0 : Grid
grid0 = { array = Array.empty, width = 0, spacing = 0 }

toVec2 : Point -> Vec2
toVec2 (x, y) = (toFloat x, toFloat y)

fromVec2 : Vec2 -> Point
fromVec2 (x, y) = (round x, round y)

size : Grid -> Int
size grid = grid.width * Array.length grid.array
repeat : Int -> Int -> Float -> GridNode -> Grid
repeat w h spacing node =
  { array = Array.repeat (w * h) node, width = w, spacing = spacing }
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

manhattan : Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

samplePoint : Grid -> Generator Point
samplePoint grid = let gridH = Array.length grid.array // grid.width in
  Random.pair (Random.int 0 (grid.width - 1)) (Random.int 0 (gridH - 1))

shuffle : Grid -> Generator Grid
shuffle grid = Random.map (\array -> { grid | array = array })
  (Random.Array.shuffle grid.array)

random : Int -> Int -> Float -> Generator Grid
random w h spacing = let
  board = Array.initialize (w * h) (\i -> case i % 4 of
    1 -> Water
    2 -> Sand
    3 -> Road
    _ -> Obstacle)
 in
  shuffle { array = board, width = w, spacing = spacing }

cost : GridNode -> Float
cost node = case node of
  Road -> 1
  Sand -> 2
  Water -> 3
  Obstacle -> 1/0

neighborFilter : Grid -> (Point, Float) -> Maybe (Point, Float)
neighborFilter grid (p, c) =
  if not (inGrid p grid)
    then Nothing
  else case get p grid of
    Obstacle -> Nothing
    node -> Just (p, c * cost node)

sqrt2 : Float
sqrt2 = sqrt 2
sqrt5 : Float
sqrt5 = sqrt 5

candidates4 : Point -> List (Point, Float)
candidates4 (x, y) =
  [ ((x, y-1), 1)
  , ((x+1, y), 1)
  , ((x, y+1), 1)
  , ((x-1, y), 1)
  ]

-- returns points on grid (ortho-)adjacent to given point with movement costs
neighbors4 : Point -> Grid -> List (Point, Float)
neighbors4 p grid = List.filterMap (neighborFilter grid) (candidates4 p)

candidates8 : Point -> List (Point, Float)
candidates8 (x, y) = candidates4 (x, y) ++
  [ ((x+1, y-1), sqrt2)
  , ((x+1, y+1), sqrt2)
  , ((x-1, y+1), sqrt2)
  , ((x-1, y-1), sqrt2)
  ]

-- returns points on grid adjacent (+ diag) to given point with movement costs
neighbors8 : Point -> Grid -> List (Point, Float)
neighbors8 p grid = List.filterMap (neighborFilter grid) (candidates8 p)

candidates12 : Point -> List (Point, Float)
candidates12 (x, y) = candidates8 (x, y) ++
  [ ((x, y-2), 2)
  , ((x+2, y), 2)
  , ((x, y+2), 2)
  , ((x-2, y), 2)
  ]

-- returns points on grid adjacent (+ diag) to given point with movement costs
neighbors12 : Point -> Grid -> List (Point, Float)
neighbors12 p grid = List.filterMap (neighborFilter grid) (candidates12 p)

candidates20 : Point -> List (Point, Float)
candidates20 (x, y) = candidates12 (x, y) ++
  [ ((x+1, y-2), sqrt5)
  , ((x-1, y-2), sqrt5)
  , ((x-2, y-1), sqrt5)
  , ((x-2, y+1), sqrt5)
  , ((x-1, y+2), sqrt5)
  , ((x+1, y+2), sqrt5)
  , ((x+2, y+1), sqrt5)
  , ((x+2, y-1), sqrt5)
  ]

neighbors20 : Point -> Grid -> List (Point, Float)
neighbors20 p grid = List.filterMap (neighborFilter grid) (candidates20 p)

grid2screen : Point -> Grid -> Vec2
grid2screen p grid = let offset = toFloat (grid.width - 1) / 2 in
  ((toVec2 p .-. (offset, offset)) .* grid.spacing)

gridIndexToScreen : Int -> Grid -> Vec2
gridIndexToScreen i grid = grid2screen (deindex i grid) grid

screen2grid : Vec2 -> Grid -> Point
screen2grid s grid = let offset = toFloat (grid.width - 1) / 2 in
  fromVec2 (s ./ grid.spacing .+. (offset, offset))

drawGrid : Grid -> List Form
drawGrid grid = ArrayToList.indexedMap (
    \i node -> filled (case node of
      Road -> lightCharcoal
      Sand -> lightBrown
      Water -> lightBlue
      Obstacle -> darkCharcoal) (square (grid.spacing * 0.9)) |>
    move (gridIndexToScreen i grid)
  ) grid.array
