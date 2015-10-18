module PathFollowing where

import Vec2 exposing (..)
import ClassicalEngine exposing (Actor, stepActor, drawVehicle)
import Grid exposing (Grid, Point,
  gridPointToScreen, screenPointToGrid, drawGrid)
import Heap
import ChaseEvade exposing (chase, arrive)
import PathFinding exposing (AStarState, initSearch, aStarStep, traceCrumbs)
import Random exposing (Seed, Generator, generate)
import Time exposing (Time, inSeconds)
import Color exposing (Color, yellow, green, red, grey)
import Graphics.Collage exposing (Form, move)
import Text exposing (fromString)
import Debug

--- CONSTANTS ---

gridW = 20
gridH = 20
spacing = 25
maxV = 40
numExplorers = 15


--- STRUCTURES ---

type alias Path = List Point

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
 in case e.state of
  Plotting goal state -> case state.frontier of
    Heap.Leaf -> { e | state <- Seeking (traceCrumbs goal state.breadcrumbs grid) }
    Heap.Node _ _ -> { e | state <- Plotting goal (aStarStep goal grid state) }
  Seeking (next :: rest) -> if p == next
   then { e | state <- Seeking rest }
   else { e
    | state <- Seeking (next :: rest)
    , vehicle <- chase (gridPointToScreen next grid) e.vehicle
    }
  Seeking [goal] -> { e | state <- Arriving goal }
  Seeking [] -> { e | state <- Resting }
  Arriving goal -> if (e.vehicle.v == (0.0, 0.0))
   then { e | state <- Resting }
   else { e | vehicle <- e.vehicle |> arrive (gridPointToScreen goal grid) }
  _ -> e


--- SIMULATION ---

stepExplorer : Float -> Explorer -> Explorer
stepExplorer dt e = { e | vehicle <- stepActor maxV dt e.vehicle }

initSim : Seed -> Simulation
initSim seed0 = let
  emptyGrid = Grid.repeat gridW gridH spacing Grid.Traversable
  randp = Grid.rand emptyGrid
  (points, seed1) = generate (Random.list numExplorers randp) seed0
 in
  { grid = emptyGrid
  , rand = randp
  , seed = seed1
  , explorers = List.map (\p ->
     { vehicle = { pos = gridPointToScreen p emptyGrid, v = (0, 0), a = (0, 0) }
     , state = Resting
     }) points
  }

simulate : Time -> Simulation -> Simulation
simulate t sim = let dt = inSeconds t in { sim
 | explorers <- Debug.watch "explorers" (List.foldl (\e (list, seed0) ->
     let new_e = explore (stepExplorer dt e) sim.grid in
      case new_e.state of
        Resting -> let (goal, seed1) = generate sim.rand seed0 in
          ({ new_e | state <- Plotting goal (initSearch goal sim.grid) } :: list, seed1)
        _ -> (new_e :: list, seed0)
    ) ([], sim.seed) sim.explorers |> fst)
 }


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
