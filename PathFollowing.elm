module PathFollowing exposing (..)

import ClassicalEngine exposing (Actor, stepActor, drawVehicle)
import Grid exposing (Grid, GridNode(..), Point, Path,
  grid2screen, screen2grid, neighbors8, drawGrid)
import ChaseEvade exposing (chase, arrive, drawTarget)
import PathFinding exposing (AStarState,
  initSearch, aStar, drawPath, drawRunningCosts)
import Random exposing (Seed, Generator, generate)
import Random.Array exposing (shuffle)
import Time exposing (Time, inSeconds)
import Color exposing (Color, yellow, green, red, grey)
import Collage exposing (Form, circle, solid, filled, move, collage)
import Array exposing (slice)
import Html exposing (Html)
import Element exposing (toHtml)
import List.Extra as List


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
  _ -> 80 / Grid.cost node
maxA : Float
maxA = 280
numExplorers : Int
numExplorers = 15


--- STRUCTURES ---

type Action = Init Simulation | Tick Time | Plot Point

type Exploration = Plotting Point | Seeking Path | Arriving Point | Resting

type alias SearchState etc = { etc
 | state : Exploration
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
  p = screen2grid e.pos grid
  node = Grid.get p grid
 in case e.state of
  Plotting goal -> { e | state = Seeking (aStar neighbors8 grid p goal) }
  Seeking [] -> { e | state = Resting }
  Seeking [goal] -> { e | state = Arriving goal }
  Seeking (next :: rest) -> let path = next :: rest in
    case List.dropWhile ((/=) p) path of
      [] -> e |> chase (maxV node) maxA (grid2screen next grid)
      [goal] -> { e | state = Arriving goal }
      _ :: truncatedPath -> { e | state = Seeking truncatedPath }
  Arriving goal -> if p == goal
   then { e | state = Resting }
   else e |> arrive (maxV node) maxA (grid2screen goal grid)
  Resting -> e |> arrive (maxV node) maxA (grid2screen p grid)


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
        { pos = grid2screen p grid
        , v = (0, 0)
        , a = (0, 0)
        , state = Resting
        }
      ) points
    }
  ) genGrid genIndices

simulate : Time -> Simulation -> (Simulation, Cmd Action)
simulate t sim = let
  dt = inSeconds t
  (new_explorers, cmd) = List.foldr (\e (list, cmd) -> let
      node = Grid.get (screen2grid e.pos sim.grid) sim.grid
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
      then ({ e | state = Plotting goal } :: es, True)
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
      Plotting goal -> drawTarget (solid red) (grid2screen goal sim.grid)
      Seeking path -> [drawPath path sim.grid]
      Arriving goal -> drawTarget (solid red) (grid2screen goal sim.grid)
      Resting -> []
    Nothing -> []
