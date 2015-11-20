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
import Color exposing (..)
import Graphics.Collage exposing (Form, text, move)

--- CONSTANTS ---

gridW = 20
gridH = 20
spacing = 30

type KeyColor = R | G | B
type Prop = Key KeyColor | Door KeyColor Bool | Chest

initProps : List Prop
initProps =
 [ Key R, Key G, Key B
 , Door R False, Door G False, Door B False
 , Chest
 ]

indexProp : Int -> Prop
indexProp i = case List.drop i initProps of
  prop :: _ -> prop
  [] -> Chest

states : Array String
states = Array.fromList
  [ "Seek Keys"
  , "Seek Doors"
  , "Seek Treasure"
  , "Celebrate !!"
  ]
 
--- STRUCTURES ---

type alias Explorer = PathFollowing.Explorer { inv : List Prop }

type alias Dungeon =
 { floor : Grid
 , loot : List (Prop, Point)
 , explorer : Explorer
 }

type alias Simulation = StateMachine Dungeon


--- BEHAVIOR ---

isKey prop = case prop of
  Key _ -> True
  _ -> False

isLockedDoor prop = case prop of
  Door _ False -> True
  _ -> False

contains : a -> List a -> Bool
contains item list = [] /= List.filter ((==) item) list

rules : List (String, Condition Dungeon, Action Dungeon, String)
rules =
 [ ( "Seek Keys"
   , (\dungeon -> not <| List.any (fst >> isKey) dungeon.loot)
   , (\dungeon -> let e = dungeon.explorer in
       { dungeon | explorer = { e | state = Resting } }
     )
   , "Seek Doors"
   )
 , ( "Seek Keys"
   , (\dungeon -> dungeon.explorer.state == Resting)
   , (\dungeon -> let
       e = dungeon.explorer
       start = screenPointToGrid e.pos dungeon.floor
       keys = List.filter (fst >> isKey) dungeon.loot
       target = case keys of
         (_, p) :: _ -> p
         [] -> (0, 0)
      in { dungeon
       | explorer = { e
         | state = Plotting target
         , search = initSearch start dungeon.floor
         }
       }
     )
   , "Seek Keys"
   )
 , ( "Seek Doors"
   , (\dungeon -> not <| List.any (fst >> isLockedDoor) dungeon.loot)
   , (\dungeon -> let e = dungeon.explorer in
       { dungeon | explorer = { e | state = Resting } }
     )
   , "Seek Treasure"
   )
 , ( "Seek Doors"
   , (\dungeon -> dungeon.explorer.state == Resting)
   , (\dungeon -> let
       e = dungeon.explorer
       start = screenPointToGrid e.pos dungeon.floor
       lockedDoors = List.filter (fst >> isLockedDoor) dungeon.loot
       target = case lockedDoors of
         (_, p) :: _ -> p
         [] -> (0, 0)
      in { dungeon
       | explorer = { e
         | state = Plotting target
         , search = initSearch start dungeon.floor
         }
       }
     )
   , "Seek Doors"
   )
 , ( "Seek Treasure"
   , (\dungeon -> not <| List.any (fst >> (==) Chest) dungeon.loot)
   , identity
   , "Celebrate !!"
   )
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

--initSim : Seed -> Simulation
initSim seed = List.foldl addRule
  (StateMachine.new states (initDungeon seed)) rules

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = inSeconds t
  dungeon = sim.info
  grid = dungeon.floor
  e = dungeon.explorer
  e_p = (screenPointToGrid e.pos grid)
  node = Grid.get e_p grid
  new_e = explore (e |> stepActor (maxV node) dt) grid
  new_dungeon = case List.filter (\(_, p) -> p == e_p) dungeon.loot of
    (prop, p) :: _ -> case prop of
      Key c -> { dungeon
        | explorer = { new_e | inv = prop :: new_e.inv }
        , loot = List.filter ((/=) (prop, p)) dungeon.loot
       }
      Door c False -> if List.any ((==) (Key c)) new_e.inv
        then { dungeon
          | explorer = new_e
          , loot = (Door c True, p)
            :: List.filter ((/=) (prop, p)) dungeon.loot
         }
        else { dungeon | explorer = new_e }
      Chest -> if not <| List.any (fst >> isLockedDoor) dungeon.loot
        then { dungeon | loot = List.filter (fst >> (/=) Chest) dungeon.loot }
        else { dungeon | explorer = new_e }
      _ -> { dungeon | explorer = new_e }
    [] -> { dungeon | explorer = new_e }
 in StateMachine.update new_dungeon sim


--- DRAWING ---

keyColor : KeyColor -> Color
keyColor c = case c of
  R -> red
  G -> green
  B -> blue

drawProp : Prop -> Vec2 -> Form
drawProp prop xy = move xy <| text <| case prop of
  Key c -> Text.fromString "K" |> Text.color (keyColor c)
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
  ++ (case e.state of
      Plotting goal -> drawRunningCosts
        e.search.running_cost grid
      Seeking path -> [drawPath path grid]
      _ -> [])
  ++ (case Array.get sim.state states of
      Just s -> [s |> Text.fromString
        |> Text.color purple |> Text.height 18 |> Text.bold
        |> text |> move e.pos]
      Nothing -> [])