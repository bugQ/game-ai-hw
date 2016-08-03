module PathFollowing exposing (..)

import Vec2 exposing (..)
import ClassicalEngine exposing (Actor, stepActor, drawVehicle)
import Grid exposing (Grid, GridNode(Road, Sand, Water, Obstacle), Point, Path,
  gridPointToScreen, screenPointToGrid, drawGrid)
import Heap
import ChaseEvade exposing (chase, arrive, drawTarget)
import PathFinding exposing (AStarState,
  initSearch, aStarStep, traceCrumbs, drawPath, drawRunningCosts)
import Random exposing (Seed, Generator, generate)
import Random.Array exposing (shuffle)
import Time exposing (Time, inSeconds)
import Color exposing (Color, yellow, green, red, grey)
import Collage exposing (Form, circle, solid, filled, move)
import Array exposing (slice)


--- CONSTANTS ---
-- (mostly) --

gridW : Int
gridW = 20
gridH : Int
gridH = 20
spacing : Float
spacing = 30
maxV : GridNode -> Float
maxV node = case node of
  Obstacle -> 5
  _ -> 40 / Grid.cost node
maxA : Float
maxA = 70
numExplorers : Int
numExplorers = 15


--- STRUCTURES ---

type Action = Init Simulation | Tick Time | Plot Point

type Exploration = Plotting Point | Seeking Path | Arriving Point | Resting

type alias SearchState etc = { etc
 | search : AStarState
 , state : Exploration
 }

type alias Explorer etc = Actor (SearchState etc)

type alias Simulation =
 { grid : Grid
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

initSim : Generator Simulation
initSim = let
  genGrid = Grid.random gridW gridH spacing
  genIndices = shuffle (Array.initialize (gridW * gridH) identity)
 in
  Random.map2 (\grid indices -> let
    openIndices = Array.filter
      (\i -> Array.get i grid.array /= Just Obstacle) indices
    points = Array.toList openIndices |> List.take numExplorers
      |> List.map ((flip Grid.deindex) grid)
   in
    { grid = grid
    , explorers = List.map (\p ->
        { pos = gridPointToScreen p grid
        , v = (0, 0)
        , a = (0, 0)
        , search = initSearch p grid
        , state = Resting
        }
      ) points
    }
  ) genGrid genIndices

simulate : Time -> Simulation -> (Simulation, Cmd Action)
simulate t sim = let
  dt = inSeconds t
  (new_explorers, cmd) = List.foldr (\e (list, cmd) -> let
      node = Grid.get (screenPointToGrid e.pos sim.grid) sim.grid
      new_e = explore (e |> stepActor (maxV node) dt) sim.grid
     in (new_e :: list, case new_e.state of
        Resting -> Random.generate Plot (Grid.samplePoint sim.grid)
--          let (goal, seed1) = generate (Grid.samplePoint sim.grid) seed0 in
--          ({ new_e
--           | state = Plotting goal
--           , search = initSearch (screenPointToGrid e.pos sim.grid) sim.grid
--           } :: list, seed1)
        _ -> cmd
      )
    ) ([], Cmd.none) sim.explorers
 in ({ sim | explorers = new_explorers }, cmd)


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
