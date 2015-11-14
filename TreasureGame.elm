module TreasureGame where

import Vec2 exposing (Vec2)
import ClassicalEngine exposing (Actor, stepActor, drawVehicle)
import Grid exposing (Grid, Point, GridNode(Obstacle),
  deindex, screenPointToGrid, gridPointToScreen, drawGrid)
import PathFinding exposing (AStarState,
  initSearch, drawRunningCosts, drawPath)
import PathFollowing exposing (
  Exploration(Plotting, Seeking, Arriving, Resting), maxV, explore, stateColor)
import Shuffle exposing (shuffle)
import ArrayToList
import Array exposing (Array)
import Random exposing (Seed)
import Time exposing (Time, inSeconds)
import Text exposing (Text)
import Color exposing (Color, purple, red, green, blue, brown)
import Graphics.Collage exposing (Form, text, move)

--- CONSTANTS ---

gridW = 20
gridH = 20
spacing = 30


--- STRUCTURES ---

type KeyColor = R | G | B
type PropType = Key KeyColor | Door KeyColor | Chest

propTypes : Array PropType
propTypes = Array.fromList
 [ Key R, Key G, Key B
 , Door R, Door G, Door B
 , Chest
 ]

-- prop types are mapped to ints for storage
propIndex : PropType -> Int
propIndex prop = case prop of
  Key R -> 0
  Key G -> 1
  Key B -> 2
  Door R -> 3
  Door G -> 4
  Door B -> 5
  Chest -> 6

indexProp : Int -> PropType
indexProp i = Maybe.withDefault (Key R) (Array.get i propTypes)

type alias Explorer = Actor (PathFollowing.SearchState {})

type alias Dungeon =
 { floor : Grid
 , props : Array (Point, Bool) -- tracks whether prop has been activated
 }

type alias Simulation =
 { dungeon : Dungeon
 , explorer : Explorer
 , target : Int
 }

 
--- BEHAVIOR ---

initSim : Seed -> Simulation
initSim seed0 = let
  (grid, seed1) = Grid.randGrid gridW gridH spacing seed0
  indices = Array.initialize (Array.length grid.array) identity
  (randIndices, seed2) = shuffle seed1 indices
  openIndices = Array.filter
    (\i -> Array.get i grid.array /= Just Obstacle) randIndices
  props = Array.slice 1 7 openIndices |> Array.map
    (\i -> (deindex i grid, False))
  dungeon = { floor = grid, props = props }
  start = deindex (Array.get 0 openIndices |> Maybe.withDefault 0) grid
  actor = { pos = gridPointToScreen start grid, v = (0.0, 0.0), a = (0.0, 0.0) }
  targetPoint = case Array.get 0 dungeon.props of
    Just (p, _) -> p
 in
  { dungeon = dungeon
  , explorer =
    { vehicle = actor
    , search = initSearch start grid
    , state = Plotting targetPoint
    }
  , target = 0
  }

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = inSeconds t
  dungeon = sim.dungeon
  grid = dungeon.floor
  e = sim.explorer
  node = Grid.get (screenPointToGrid e.vehicle.pos grid) grid
  new_e = explore { e | vehicle <- stepActor (maxV node) dt e.vehicle } grid
  new_p = screenPointToGrid new_e.vehicle.pos grid
  new_sim = { sim | explorer <- new_e }
 in case new_e.state of
  Arriving _ -> let
    propLoc = case Array.get sim.target dungeon.props of
      Just (p, _) -> p
   in { new_sim | dungeon <- { dungeon | props <-
      Array.set sim.target (propLoc, True) dungeon.props }}
  Resting -> if sim.target == 6 then new_sim else let
    new_target = sim.target + 1
    targetPoint = case Array.get new_target dungeon.props of
      Just (p, _) -> p
   in
    { sim | explorer <- { new_e
      | state <- Plotting targetPoint
      , search <- initSearch new_p grid
      }
      , target <- new_target
    }
  _ -> new_sim


--- DRAWING ---

keyColor : KeyColor -> Color
keyColor c = case c of
  R -> red
  G -> green
  B -> blue

drawProp : PropType -> Vec2 -> Form
drawProp prop xy = move xy <| text <| case prop of
  Key c -> Text.fromString "K" |> Text.color (keyColor c)
  Door c -> Text.fromString "D" |> Text.color (keyColor c)
  Chest -> Text.fromString "X" |> Text.color brown

drawSim : Simulation -> List Form
drawSim sim = drawGrid sim.dungeon.floor
  ++ ArrayToList.indexedMap (\i (p, _) ->
        drawProp (indexProp i) (gridPointToScreen p sim.dungeon.floor)
      ) sim.dungeon.props
  ++ drawVehicle (stateColor sim.explorer.state) sim.explorer.vehicle
  ++ case sim.explorer.state of
      Plotting goal -> drawRunningCosts
        sim.explorer.search.running_cost sim.dungeon.floor
      Seeking path -> [drawPath path sim.dungeon.floor]
      _ -> []