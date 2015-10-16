module PathFinding where

import Array exposing (Array)
import Maybe exposing (withDefault)
import Graphics.Collage exposing (Form,
  traced, filled, dashed, circle, move, text)
import Text exposing (Text, fromString)
import Color exposing (red, green, lightYellow)
import ArrayToList exposing (indexedFilterMap)
import Vec2 exposing (Vec2, dist)
import Grid exposing (Grid, Point, GridNode, toVec2, inGrid,
  neighbors, gridPointToScreen, gridIndexToScreen, drawGrid)
import Random exposing (Generator, generate, Seed)
import Heap exposing (Heap, findmin, deletemin)
import Time exposing (Time)
import Debug

--- CONSTANTS ---

gridW = 15
gridH = 15
maxBlocks = 60
inf = 1.0 / 0.0

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
 , finished : Bool
 }

--- BEHAVIOR ---

initSearch : Point -> Grid -> AStarState
initSearch p0 grid = let i0 = Grid.index p0 grid in
 { frontier = Heap.insert (0.0, i0) Heap.Leaf
 , breadcrumbs = Array.repeat (Grid.size grid) -1
 , running_cost = Array.repeat (Grid.size grid) inf |> Array.set i0 0
 , finished = False
 }

findPath : Point -> Point -> Grid -> List Point
findPath p0 p1 grid = aStar p1 grid (initSearch p0 grid)

heuristic : Point -> Point -> Float
heuristic p1 p2 = dist (toVec2 p1) (toVec2 p2)

aStarStep : Point -> Grid -> AStarState -> AStarState
aStarStep goal grid state = if state.finished then state else let
  frontier = Debug.watch "frontier" state.frontier  -- view entire heap in debugger
  (_, i) = findmin frontier |> withDefault (0.0, -1)
  new_cost = (Array.get i state.running_cost |> withDefault -1) + 1
  poppedState = { state | frontier <- deletemin state.frontier }
 in
  if i < 0 then state else List.foldl (\next s ->
     let j = Grid.index next grid in
      if not s.finished && new_cost < (Array.get j s.running_cost |> withDefault -1) then
        { s
        | frontier <- Heap.insert (new_cost + heuristic next goal, j) s.frontier
        , breadcrumbs <- Array.set j i s.breadcrumbs
        , running_cost <- Array.set j new_cost s.running_cost
        , finished <- next == goal
        }
      else s)
    poppedState
    (Debug.watch "neighbors" (neighbors (Grid.deindex i grid) grid))

aStar : Point -> Grid -> AStarState -> List Point
aStar goal grid state = case state.frontier of
  Heap.Leaf -> traceCrumbs goal state.breadcrumbs grid
  Heap.Node _ _ -> aStar goal grid (aStarStep goal grid state)

traceCrumbs : Point -> Array Int -> Grid -> List Point
traceCrumbs p crumbs grid = let
  prev = Array.get (Grid.index p grid) crumbs |> withDefault -1
 in
  if prev < 0 then [p] else p :: traceCrumbs (Grid.deindex prev grid) crumbs grid

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

simulate : Time -> Simulation -> Simulation
simulate _ sim = stepSearch sim

stepSearch : Simulation -> Simulation
stepSearch sim = { sim | search <- aStarStep sim.goal sim.grid sim.search }

runSearch : Simulation -> Simulation
runSearch sim = case sim.search.frontier of
  Heap.Leaf -> sim
  Heap.Node _ _ -> stepSearch sim

--- DRAWING ---

drawFrontier : Heap (Float, Int) -> Grid -> List Form
drawFrontier heap grid = case heap of
  Heap.Leaf -> []
  Heap.Node (p, i) heaps ->
    (circle 9 |> filled lightYellow |> move (gridIndexToScreen i grid))
    :: List.foldl (\h sum -> sum ++ drawFrontier h grid) [] heaps

drawRunningCosts : Array Float -> Grid -> List Form
drawRunningCosts costs grid = List.filterMap (\i -> case Array.get i costs of
    Just c -> if c == inf then Nothing else
      Just (toString c |> fromString |> text |> move (gridIndexToScreen i grid))
    Nothing -> Nothing) [0..(Array.length grid.array)]

drawBreadcrumbPath : Point -> Array Int -> Grid -> Form
drawBreadcrumbPath goal crumbs grid = traceCrumbs goal crumbs grid |>
    List.map ((flip gridPointToScreen) grid) |> traced (dashed red)

drawSim : Simulation -> List Form
drawSim sim = drawGrid sim.grid
  ++ drawFrontier sim.search.frontier sim.grid
  ++
   [ circle 15 |> filled red |> move (gridPointToScreen sim.goal sim.grid)
   , circle 13 |> filled green |> move (gridPointToScreen sim.start sim.grid)
   , drawBreadcrumbPath sim.goal sim.search.breadcrumbs sim.grid
   ]
  ++ drawRunningCosts sim.search.running_cost sim.grid
