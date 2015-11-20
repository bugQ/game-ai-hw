module PathFinding where

import Array exposing (Array)
import Maybe exposing (withDefault)
import Graphics.Collage exposing (Form,
  traced, filled, dashed, circle, move, text, path)
import Text exposing (Text, fromString)
import Color exposing (red, green, lightYellow)
import ArrayToList exposing (indexedFilterMap)
import Vec2 exposing (Vec2, dist)
import Grid exposing (Grid, Point, GridNode, Path, toVec2, inGrid,
  neighbors, gridPointToScreen, gridIndexToScreen, drawGrid)
import Random exposing (Seed, Generator, generate)
import Heap exposing (Heap, findmin, deletemin)
import Time exposing (Time)
import String exposing (left)
import Debug


--- CONSTANTS ---

gridW = 15
gridH = 15
spacing = 40
maxBlocks = 90
inf = 1.0 / 0.0


--- STRUCTURES ---

type alias Simulation =
 { grid : Grid
 , start : Point
 , goal : Point
 , search : AStarState
 , rand : Generator Point
 , seed : Seed
 , restart : Int
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
  frontier = state.frontier  -- view entire heap in debugger
  (_, i) = findmin frontier |> withDefault (0.0, -1)
  current_cost = (Array.get i state.running_cost |> withDefault -1)
  poppedState = { state | frontier = deletemin state.frontier }
 in
  if i < 0 then { state | finished = True }
    else List.foldl (\(next, next_cost) s ->
     let
      j = Grid.index next grid
      new_cost = current_cost + next_cost
     in
      if not s.finished && new_cost < (Array.get j s.running_cost |> withDefault -1) then
        { s
        | frontier = Heap.insert (new_cost + heuristic next goal, j) s.frontier
        , breadcrumbs = Array.set j i s.breadcrumbs
        , running_cost = Array.set j new_cost s.running_cost
        , finished = next == goal
        }
      else s)
    poppedState
    (neighbors (Grid.deindex i grid) grid)

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
  emptyGrid = Grid.repeat gridW gridH spacing Grid.Road
  randp = Grid.rand emptyGrid
  (indices, seed1) = generate (Random.list maxBlocks randp) seed0
  grid = List.foldr ((flip Grid.set) Grid.Obstacle) emptyGrid indices
  openNodes = indexedFilterMap (\i node -> case node of
    Grid.Road -> Just i
    _ -> Nothing) grid.array
  randn = Random.int 0 (List.length openNodes)
  (startn, seed2) = generate randn seed1
  (goaln, seed3) = generate randn seed2
  start = Grid.deindex (List.drop startn openNodes |> List.head |> withDefault 0) grid
  goal = Grid.deindex (List.drop goaln openNodes |> List.head |> withDefault 0) grid
 in
  { grid = grid
  , start = start
  , goal = goal
  , search = initSearch start grid
  , rand = randp
  , seed = seed3
  , restart = 20
  }

simulate : Time -> Simulation -> Simulation
simulate _ sim = if sim.restart <= 0 then initSim sim.seed else let s = stepSearch sim in
  { s | restart = if s.search.finished then s.restart - 1 else s.restart }

stepSearch : Simulation -> Simulation
stepSearch sim = { sim | search = aStarStep sim.goal sim.grid sim.search }

runSearch : Simulation -> Simulation
runSearch sim = case sim.search.frontier of
  Heap.Leaf -> sim
  Heap.Node _ _ -> stepSearch sim


--- DRAWING ---

drawFrontier : Heap (Float, Int) -> Grid -> List Form
drawFrontier heap grid = case heap of
  Heap.Leaf -> []
  Heap.Node (p, i) heaps ->
    (circle 12 |> filled lightYellow |> move (gridIndexToScreen i grid))
     :: List.foldl (\h sum -> sum ++ drawFrontier h grid) [] heaps

drawRunningCosts : Array Float -> Grid -> List Form
drawRunningCosts costs grid = List.filterMap (\i -> case Array.get i costs of
    Just c -> if c == inf then Nothing else
      Just (toString c |> left 4 |> fromString |> text |> move (gridIndexToScreen i grid))
    Nothing -> Nothing) [0..(Array.length grid.array)]

drawPath : Path -> Grid -> Form
drawPath pp grid = List.map ((flip gridPointToScreen) grid) pp
  |> path |> traced (dashed red)

drawBreadcrumbPath : Point -> Array Int -> Grid -> Form
drawBreadcrumbPath goal crumbs grid = drawPath (traceCrumbs goal crumbs grid) grid

drawSim : Simulation -> List Form
drawSim sim = drawGrid sim.grid
  ++ drawFrontier sim.search.frontier sim.grid
  ++
   [ circle 19 |> filled red |> move (gridPointToScreen sim.goal sim.grid)
   , circle 17 |> filled green |> move (gridPointToScreen sim.start sim.grid)
   , drawBreadcrumbPath sim.goal sim.search.breadcrumbs sim.grid
   ]
  ++ drawRunningCosts sim.search.running_cost sim.grid
