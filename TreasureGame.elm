module TreasureGame where

import Vec2 exposing (Vec2)
import ClassicalEngine exposing (Actor, stepActor, drawVehicle)
import Grid exposing (Grid, Point, GridNode(Obstacle),
  deindex, screenPointToGrid, gridPointToScreen, drawGrid)
import PathFinding exposing (AStarState,
  initSearch, drawRunningCosts, drawPath)
import PathFollowing exposing (
  Exploration(Plotting, Seeking, Arriving, Resting), maxV, explore, stateColor)
import StateMachine exposing (State, Condition, Action, StateMachine,
  addRule, apprise)
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

type KeyColor = R | G | B
type Prop = Key KeyColor Bool | Door KeyColor Bool | Chest

initProps : List Prop
initProps =
 [ Key R False, Key G False, Key B False
 , Door R False, Door G False, Door B False
 , Chest
 ]

indexProp : Int -> Prop
indexProp i = let (prop :: _) = List.drop i initProps in prop

 
--- STRUCTURES ---

type alias Explorer = PathFollowing.Explorer { inv : List Prop }

type alias Dungeon =
 { floor : Grid
 , loot : List (Prop, Point)
 , explorer : Explorer
 }

type alias Simulation = StateMachine Dungeon


--- BEHAVIOR ---

count : (a -> Bool) -> List a -> Int
count cond list = List.filter cond list |> List.length 

contains : a -> List a -> Bool
contains item list = [] /= List.filter ((==) item) list

rules : List (String, Condition Dungeon, Action Dungeon, String)
rules =
 [ ( "Seek Keys"
   , (\dungeon -> 0 == count (\prop -> case prop of
       Key _ _ -> True
       _ -> False) dungeon.explorer.inv)
   , identity
   , "Seek Doors" )
 ]

initDungeon : Seed -> Dungeon
initDungeon seed0 = let
  (grid, seed1) = Grid.newRand gridW gridH spacing seed0
  indices = Array.initialize (Array.length grid.array) identity
  (randIndices, seed2) = shuffle seed1 indices
  openIndices = Array.filter
    (\i -> Array.get i grid.array /= Just Obstacle) randIndices
  props = Array.slice 1 (List.length initProps) openIndices
    |> ArrayToList.map ((flip deindex) grid) |> List.map2 (,) initProps
  start = deindex (Array.get 0 openIndices |> Maybe.withDefault 0) grid
 in
  { floor = grid
  , loot = props
  , explorer =
    { pos = gridPointToScreen start grid
    , v = (0.0, 0.0)
    , a = (0.0, 0.0)
    , search = initSearch start grid
    , state = Resting
    , inv = []
    }
  }

initSim : Seed -> Simulation
initSim seed = StateMachine.new 
  [ "Seek Keys"
  , "Seek Doors"
  , "Seek Treasure"
  ] (initDungeon seed)

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = inSeconds t
  dungeon = sim.info
  grid = dungeon.floor
  e = dungeon.explorer
  node = Grid.get (screenPointToGrid e.pos grid) grid
  new_e = explore (e |> stepActor (maxV node) dt) grid
  new_dungeon = { dungeon | explorer <- new_e }
 in StateMachine.update new_dungeon sim


--- DRAWING ---

keyColor : KeyColor -> Color
keyColor c = case c of
  R -> red
  G -> green
  B -> blue

drawProp : Prop -> Vec2 -> Form
drawProp prop xy = move xy <| text <| case prop of
  Key c used -> Text.fromString "K" |> Text.color (keyColor c)
  Door c open -> Text.fromString (if open then "O" else "D")
    |> Text.color (keyColor c)
  Chest -> Text.fromString "X" |> Text.color brown

drawSim : Simulation -> List Form
drawSim sim = let
  grid = sim.info.floor
  props = sim.info.loot
  e = sim.info.explorer
 in drawGrid grid
  ++ List.map (\(prop, p) ->
        drawProp prop (gridPointToScreen p grid)
      ) props
  ++ drawVehicle (stateColor e.state) e
  ++ case e.state of
      Plotting goal -> drawRunningCosts
        e.search.running_cost grid
      Seeking path -> [drawPath path grid]
      _ -> []