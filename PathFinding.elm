module PathFinding where

import Array exposing (Array)
import Maybe exposing (withDefault)
import Graphics.Collage exposing (Form,
  traced, filled, dashed, circle, move)
import Color exposing (red)
import ArrayToList exposing (indexedFilterMap)
import Vec2 exposing (Vec2, dist)
import Grid exposing (Grid, Point, GridNode,
  toVec2, neighbors, gridPointToScreen, drawGrid)
import Random exposing (Generator, generate, Seed)
import Heap exposing (Heap, findmin, deletemin)

--- CONSTANTS ---

gridW = 15
gridH = 15
maxBlocks = 60

--- STRUCTURES ---

type alias Simulation =
 { grid : Grid
 , start : Point
 , goal : Point
 , search : AStarState
 , rand : Generator Point
 , seed : Seed
 }

type alias AStarState =
 { frontier : Heap (Float, Int)
 , breadcrumbs : Array Int
 , running_cost : Array Float
 }

--- BEHAVIOR ---

initSearch : Point -> Grid -> AStarState
initSearch p0 grid =
 { frontier = Heap.insert (0.0, Grid.index p0 grid) Heap.Leaf
 , breadcrumbs = Array.repeat (Grid.size grid) -1
 , running_cost = Array.repeat (Grid.size grid) -1.0
 }

findPath : Point -> Point -> Grid -> List Point
findPath p0 p1 grid = aStar p1 grid (initSearch p0 grid)

heuristic : Point -> Point -> Float
heuristic p0 p1 = dist (toVec2 p0) (toVec2 p1)

aStarStep : Point -> Grid -> AStarState -> AStarState
aStarStep p1 grid state = let
  (_, i) = findmin state.frontier |> withDefault (0.0, -1)
  poppedState = { state | frontier <- deletemin state.frontier }
 in List.foldl (\next s -> let
      j = Grid.index next grid
      new_cost = (Array.get i s.running_cost |> withDefault -1) + 1
     in
      if new_cost < (Array.get j s.running_cost |> withDefault -1) then
        { s
        | frontier <- Heap.insert (new_cost + heuristic next p1, j) s.frontier
        , breadcrumbs <- Array.set j i state.breadcrumbs
        , running_cost <- Array.set j new_cost state.running_cost
        }
      else s)
    poppedState
    (neighbors (Grid.deindex i grid))

aStar : Point -> Grid -> AStarState -> List Point
aStar p1 grid state = case state.frontier of
  Heap.Leaf -> traceCrumbs p1 state.breadcrumbs grid
  Heap.Node element heaps -> aStar p1 grid (aStarStep p1 grid state)

traceCrumbs : Point -> Array Int -> Grid -> List Point
traceCrumbs p crumbs grid =
 case Array.get (Grid.index p grid) crumbs of
  Just prev -> if prev < 0 then [] else
    p :: traceCrumbs (Grid.deindex prev grid) crumbs grid

--- SIMULATION ---

initSim : Seed -> Simulation
initSim seed0 = let
  emptyGrid = Grid.repeat gridW gridH Grid.Traversable
  randp = Random.pair (Random.int 0 gridW) (Random.int 0 gridH)
  (indices, seed1) = generate (Random.list maxBlocks randp) seed0
  grid = List.foldr Grid.set emptyGrid indices
  openNodes = indexedFilterMap (\i node -> case node of
    Grid.Untraversable -> Nothing
    Grid.Traversable -> Just i) grid.array
  randi = Random.int 0 (List.length openNodes)
  (starti, seed2) = generate randi seed1
  (goali, seed3) = generate randi seed2
  start = Grid.deindex starti grid
  goal = Grid.deindex goali grid
 in
  { grid = grid
  , start = start
  , goal = goal
  , search = initSearch start grid
  , rand = randp
  , seed = seed3
  }

drawSim : Simulation -> List Form
drawSim sim = drawGrid sim.grid ++
 [ traceCrumbs sim.goal sim.search.breadcrumbs sim.grid |>
    List.map gridPointToScreen |> traced (dashed red)
 , circle 15 |> filled red |> move (gridPointToScreen sim.goal sim.grid)
 ]
