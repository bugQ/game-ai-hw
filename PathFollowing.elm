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
import Time exposing (Time, inSeconds)
import Color exposing (Color, yellow, green, red, grey)
import Graphics.Collage exposing (Form, circle, solid, filled, move)
import Text exposing (fromString)
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

type Exploration = Plotting Point AStarState | Seeking Path | Arriving Point | Resting

type alias Explorer =
 { vehicle : Actor
 , state : Exploration
 }

type alias Simulation =
 { grid : Grid
  , rand : Generator Point
  , seed : Seed
  , explorers : List Explorer
 }


--- BEHAVIOR ---

explore : Explorer -> Grid -> Explorer
explore e grid = let
  p = screenPointToGrid e.vehicle.pos grid
  node = Grid.get p grid
 in case e.state of
  Plotting goal search -> { e | state <- case search.frontier of
    Heap.Leaf -> Resting
    Heap.Node _ _ -> if search.finished
      then Seeking (traceCrumbs goal search.breadcrumbs grid |> List.reverse)
      else Plotting goal (aStarStep goal grid search) }
  Seeking [] -> { e | state <- Resting }
  Seeking [goal] -> { e | state <- Arriving goal }
  Seeking (next :: rest) -> if p == next
   then { e | state <- Seeking rest }
   else { e
    | state <- Seeking (next :: rest)
    , vehicle <- chase (maxV node) maxA (gridPointToScreen next grid) e.vehicle
    }
  Arriving goal -> if norm e.vehicle.v < 0.01
   then { e | state <- Resting }
   else { e | vehicle <- arrive (maxV node) maxA (gridPointToScreen goal grid) e.vehicle }
  _ -> e


--- SIMULATION ---

initSim : Seed -> Simulation
initSim seed0 = let
  emptyGrid = Grid.repeat gridW gridH spacing Grid.Road
  randp = Grid.rand emptyGrid
  (points, seed1) = generate (Random.list numExplorers randp) seed0
 in
  { grid = emptyGrid
  , rand = randp
  , seed = seed1
  , explorers = List.map (\p ->
      { vehicle = { pos = gridPointToScreen p emptyGrid, v = (0, 0), a = (0, 0) }
      , state = Resting
      }
    ) points
  }

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = inSeconds t
  (new_explorers, new_seed) = List.foldr (\e (list, seed0) ->
     let
       node = Grid.get (screenPointToGrid e.vehicle.pos sim.grid) sim.grid
       new_e = explore { e | vehicle <- e.vehicle |> stepActor (maxV node) dt } sim.grid
      in case new_e.state of
        Resting -> let (goal, seed1) = generate sim.rand seed0 in
          ({ new_e | state <- Plotting goal (initSearch
              (screenPointToGrid e.vehicle.pos sim.grid) sim.grid) } :: list, seed1)
        _ -> (new_e :: list, seed0)
    ) ([], sim.seed) sim.explorers
 in { sim | explorers <- new_explorers, seed <- new_seed }


--- DRAWING ---

stateColor : Exploration -> Color
stateColor state = case state of
  Plotting _ _ -> yellow
  Seeking _ -> green
  Arriving _ -> red
  Resting -> grey

drawSim : Simulation -> List Form
drawSim sim = drawGrid sim.grid
  ++ List.foldl (++) []
    (List.indexedMap (\i e -> (e.vehicle |> drawVehicle (stateColor e.state))) sim.explorers)
  ++ case List.head sim.explorers of
    Just e -> (circle 4 |> filled red |> move e.vehicle.pos) :: case e.state of
      Plotting goal search -> drawTarget (solid red) (gridPointToScreen goal sim.grid)
        ++ drawRunningCosts search.running_cost sim.grid
      Seeking path -> [drawPath path sim.grid]
      Arriving goal -> drawTarget (solid red) (gridPointToScreen goal sim.grid)
      Resting -> []
    Nothing -> []
