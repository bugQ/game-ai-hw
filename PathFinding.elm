module PathFinding exposing (..)

import Array exposing (Array)
import Maybe exposing (withDefault)
import Collage exposing (Form,
  traced, filled, dashed, circle, move, text, path)
import Text exposing (Text, fromString)
import Color exposing (red, green, lightYellow)
import ArrayToList exposing (indexedFilter)
import Grid exposing (Grid, grid0, Point, GridNode, Path, inGrid,
  manhattan, neighbors4, grid2screen, gridIndexToScreen, drawGrid)
import Random exposing (Seed, Generator, generate)
import Heap exposing (Heap, findmin, deletemin)
import String exposing (left)


--- CONSTANTS ---

gridW : Int
gridW = 15
gridH : Int
gridH = 15
spacing : Float
spacing = 40
maxBlocks : Int
maxBlocks = 90
inf : Float
inf = 1/0


--- STRUCTURES ---

type alias Simulation =
 { grid : Grid
 , start : Point
 , goal : Point
 , search : AStarState
 , restart : Int
 }

sim0 : Simulation
sim0 =
  { grid = grid0
  , start = (0, 0)
  , goal = (0, 0)
  , search = state0
  , restart = 0
  }

type alias AStarState =
 { frontier : Heap (Float, Int)
 , breadcrumbs : Array Int
 , running_cost : Array Float
 , finished : Bool
 }

state0 : AStarState
state0 =
  { frontier = Heap.Leaf
  , breadcrumbs = Array.empty
  , running_cost = Array.empty
  , finished = True
  }

type alias Heuristic = Point -> Point -> Float -> Float

--- BEHAVIOR ---

initSearch : Point -> Grid -> AStarState
initSearch p0 grid = let i0 = Grid.index p0 grid in
 { frontier = Heap.insert (0.0, i0) Heap.Leaf
 , breadcrumbs = Array.repeat (Grid.size grid) -1
 , running_cost = Array.repeat (Grid.size grid) inf |> Array.set i0 0
 , finished = False
 }

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
      neighbor_cost = Array.get j s.running_cost |> withDefault -1
      new_cost = current_cost + next_cost
      heuristic = toFloat (manhattan next goal)
     in
      if not s.finished && (new_cost < neighbor_cost) then
        { s
        | frontier = Heap.insert (new_cost + heuristic, j) s.frontier
        , breadcrumbs = Array.set j i s.breadcrumbs
        , running_cost = Array.set j new_cost s.running_cost
        , finished = next == goal
        }
      else s)
    poppedState
    (neighbors4 (Grid.deindex i grid) grid)

aStar : Grid -> Point -> Point -> List Point
aStar grid start goal = let
  init = initSearch start grid
  step = aStarStep goal grid
  return = traceCrumbs goal grid << .breadcrumbs
  loop s = if s.finished then return s else loop (step s)
 in
  loop init

traceCrumbs : Point -> Grid -> Array Int -> List Point
traceCrumbs p grid crumbs = Array.foldl (\_ (trail, p) -> let
    prev = Array.get (Grid.index p grid) crumbs |> withDefault -1
   in
    if prev < 0 then (trail, p) else (p :: trail, Grid.deindex prev grid)
  ) ([], p) crumbs |> fst |> List.reverse

type alias PropagateState = { frontier : Heap (Float, Int), heat : Array Float }

initHeat : Float -> Point -> Grid -> PropagateState
initHeat heat p0 grid = let i0 = Grid.index p0 grid in
  { frontier = Heap.insert (heat, i0) Heap.Leaf
  , heat = Array.repeat (Grid.size grid) (0) |> Array.set i0 heat
  }

propagate : Grid -> PropagateState -> PropagateState
propagate grid state = case findmin state.frontier of
  Nothing -> state
  Just (current_heat, i) ->
    List.foldl (\(next, _) s -> let
      j = Grid.index next grid
      neighbor_heat = Array.get j s.heat |> withDefault (0)
      neighbor_cost = Grid.cost (Grid.get (Grid.deindex j grid) grid)
      new_heat = current_heat * 0.9 ^ neighbor_cost
     in
      if neighbor_heat < new_heat then
        { s
        | frontier = Heap.insert (new_heat, j) s.frontier
        , heat = Array.set j new_heat s.heat
        }
      else s
    ) { state | frontier = Heap.deletemin state.frontier }
    (neighbors4 (Grid.deindex i grid) grid)

heatMap : Grid -> Float -> Point -> Array Float
heatMap grid heat source = let
  init = initHeat heat source grid
  step = propagate grid
  return = .heat
  loop s = if s.frontier == Heap.Leaf then return s else loop (step s)
 in
  loop init


--- SIMULATION ---

initSim : Generator Simulation
initSim = let
  emptyGrid = Grid.repeat gridW gridH spacing Grid.Road
  genIndices = Random.list maxBlocks (Grid.samplePoint emptyGrid)
  genGrid = Random.map
    (\indices -> List.foldr ((flip Grid.set) Grid.Obstacle) emptyGrid indices)
    genIndices
  genIndex = Random.int 0 maxBlocks
 in
  Random.map3 (\grid i_start i_goal -> let
    openNodes = Array.fromList <|
      indexedFilter (\i node -> case node of
        Grid.Road -> Just i
        _ -> Nothing
      ) grid.array
    start = Grid.deindex (Array.get i_start openNodes |> Maybe.withDefault 0) grid
    goal = Grid.deindex (Array.get i_goal openNodes |> Maybe.withDefault 0) grid
   in
    { grid = grid
    , start = start
    , goal = goal
    , search = initSearch start grid
    , restart = 20
    }
  ) genGrid genIndex genIndex

simulate : Simulation -> Simulation
simulate sim = if sim.restart <= 0 then sim else let s = stepSearch sim in
  { s | restart = if s.search.finished then s.restart - 1 else s.restart }

stepSearch : Simulation -> Simulation
stepSearch sim = { sim | search = aStarStep sim.goal sim.grid sim.search }


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
drawPath pp grid = List.map ((flip grid2screen) grid) pp
  |> path |> traced (dashed red)

drawBreadcrumbPath : Point -> Array Int -> Grid -> Form
drawBreadcrumbPath goal crumbs grid = drawPath (traceCrumbs goal grid crumbs) grid

drawSim : Simulation -> List Form
drawSim sim = drawGrid sim.grid
  ++ drawFrontier sim.search.frontier sim.grid
  ++
   [ circle 19 |> filled red |> move (grid2screen sim.goal sim.grid)
   , circle 17 |> filled green |> move (grid2screen sim.start sim.grid)
   , drawBreadcrumbPath sim.goal sim.search.breadcrumbs sim.grid
   ]
  ++ drawRunningCosts sim.search.running_cost sim.grid
