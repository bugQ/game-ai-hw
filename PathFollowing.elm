module PathFollowing where

import Vec2 exposing (..)
import ClassicalEngine exposing (Actor, stepActor, drawVehicle)
import Grid exposing (Grid, GridNode(Road, Sand, Water, Obstacle), Point, Path,
  gridPointToScreen, screenPointToGrid, drawGrid)
import Heap
import ChaseEvade exposing (chase, arrive, drawTarget)
import PathFinding exposing (AStarState,
  initSearch, aStarStep, traceCrumbs, drawPath, drawRunningCosts)
import Random exposing (Seed, Generator, generate)
import Shuffle exposing (shuffle)
import Time exposing (Time, inSeconds)
import Color exposing (Color, yellow, green, red, grey)
import Graphics.Collage exposing (Form, circle, solid, filled, move)
import Text exposing (fromString)
import Array exposing (slice)
import Debug

--- CONSTANTS ---
-- (mostly) --

gridW = 20
gridH = 20
spacing = 30
maxV node = case node of
  Obstacle -> 5
  _ -> 40 / Grid.cost node
maxA = 70
numExplorers = 15


--- STRUCTURES ---

type Exploration = Plotting Point | Seeking Path | Arriving Point | Resting

type alias SearchState etc = { etc
 | search : AStarState
 , state : Exploration
 }

type alias Explorer etc = Actor (SearchState etc)

type alias Simulation =
 { grid : Grid
 , seed : Seed
 , explorers : List (Explorer {})
 }


--- BEHAVIOR ---

explore : Explorer etc -> Grid -> Explorer etc
explore e grid = let
  p = screenPointToGrid e.pos grid
  node = Grid.get p grid
 in case e.state of
  Plotting goal -> case e.search.frontier of
    Heap.Leaf -> { e | state = Resting }
    Heap.Node _ _ -> if e.search.finished
      then { e | state = Seeking
          (traceCrumbs goal e.search.breadcrumbs grid |> List.reverse) }
      else { e | search = aStarStep goal grid e.search }
  Seeking [] -> { e | state = Resting }
  Seeking [goal] -> { e | state = Arriving goal }
  Seeking (next :: rest) -> if p == next
   then { e | state = Seeking rest }
   else { e | state = Seeking (next :: rest) }
     |> chase (maxV node) maxA (gridPointToScreen next grid)
  Arriving goal -> if norm e.v < 0.01
   then { e | state = Resting }
   else e |> arrive (maxV node) maxA (gridPointToScreen goal grid)
  _ -> e


--- SIMULATION ---

initSim : Seed -> Simulation
initSim seed0 = let
  (grid, seed1) = Grid.newRand gridW gridH spacing seed0
  indices = Array.initialize (Array.length grid.array) identity
  (randIndices, seed2) = shuffle seed1 indices
  openIndices = Array.filter
    (\i -> Array.get i grid.array /= Just Obstacle) randIndices
  points = Array.toList openIndices |> List.take numExplorers
    |> List.map ((flip Grid.deindex) grid)
 in
  { grid = grid
  , seed = seed2
  , explorers = List.map (\p ->
      { pos = gridPointToScreen p grid
      , v = (0, 0)
      , a = (0, 0)
      , search = initSearch p grid
      , state = Resting
      }
    ) points
  }

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = inSeconds t
  (new_explorers, new_seed) = List.foldr (\e (list, seed0) ->
     let
       node = Grid.get (screenPointToGrid e.pos sim.grid) sim.grid
       new_e = explore (e |> stepActor (maxV node) dt) sim.grid
      in case new_e.state of
        Resting -> let (goal, seed1) = generate (Grid.rand sim.grid) seed0 in
          ({ new_e
           | state = Plotting goal
           , search = initSearch (screenPointToGrid e.pos sim.grid) sim.grid
           } :: list, seed1)
        _ -> (new_e :: list, seed0)
    ) ([], sim.seed) sim.explorers
 in { sim | explorers = new_explorers, seed = new_seed }


--- DRAWING ---

stateColor : Exploration -> Color
stateColor state = case state of
  Plotting _ -> yellow
  Seeking _ -> green
  Arriving _ -> red
  Resting -> grey

drawSim : Simulation -> List Form
drawSim sim = drawGrid sim.grid
  ++ List.foldl (++) []
    (List.indexedMap (\i e -> e |> drawVehicle (stateColor e.state)) sim.explorers)
  ++ case List.head sim.explorers of
    Just e -> (circle 4 |> filled red |> move e.pos) :: case e.state of
      Plotting goal -> drawTarget (solid red) (gridPointToScreen goal sim.grid)
        ++ drawRunningCosts e.search.running_cost sim.grid
      Seeking path -> [drawPath path sim.grid]
      Arriving goal -> drawTarget (solid red) (gridPointToScreen goal sim.grid)
      Resting -> []
    Nothing -> []
