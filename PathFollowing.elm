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
import Collage exposing (Form, circle, solid, filled, move, collage)
import Array exposing (slice)
import Html exposing (Html)
import Element exposing (toHtml)


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

sim0 : Simulation
sim0 = { grid = Grid.grid0, explorers = [] }

--- BEHAVIOR ---

explore : Grid -> Explorer etc -> Explorer etc
explore grid e = let
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
  Arriving goal -> if p == goal
   then { e | state = Resting }
   else e |> arrive (maxV node) maxA (gridPointToScreen goal grid)
  _ -> e

fastExplore : Grid -> Explorer etc -> Explorer etc
fastExplore grid e = let new_e = explore grid e in case e.state of
  Plotting goal -> fastExplore grid new_e
  _ -> new_e


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
      new_e = e |> stepActor (maxV node) dt |> explore sim.grid
     in (new_e :: list, case new_e.state of
        Resting -> Random.generate Plot (Grid.samplePoint sim.grid)
        _ -> cmd
      )
    ) ([], Cmd.none) sim.explorers
 in ({ sim | explorers = new_explorers }, cmd)

startPlot : Point -> Simulation -> Simulation
startPlot goal sim = { sim | explorers =
  fst <| List.foldr (\e (es, done) ->
    if not done && e.state == Resting
      then (
          { e
          | state = Plotting goal
          , search = initSearch (screenPointToGrid e.pos sim.grid) sim.grid
          } :: es
        , True )
      else (e :: es, done)
  ) ([], False) sim.explorers }

update : Action -> Simulation -> (Simulation, Cmd Action)
update action sim = case action of
  Init sim -> (sim, Cmd.none)
  Tick t -> simulate t sim
  Plot p -> (startPlot p sim, Cmd.none)

--- DRAWING ---

stateColor : Exploration -> Color
stateColor state = case state of
  Plotting _ -> yellow
  Seeking _ -> green
  Arriving _ -> red
  Resting -> grey

drawSim : Simulation -> Html Action
drawSim sim = toHtml <| collage 600 600 <| drawGrid sim.grid
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
