module TreasureGame exposing (..)

import Vec2 exposing (Vec2)
import ClassicalEngine exposing (Actor, stepActor, drawVehicle)
import Grid exposing (Grid, Point, GridNode(Obstacle),
  deindex, screenPointToGrid, gridPointToScreen, drawGrid)
import PathFinding exposing (AStarState, state0,
  initSearch, drawRunningCosts, drawPath)
import PathFollowing exposing (
  Exploration(Plotting, Seeking, Arriving, Resting), maxV, explore, stateColor)
import StateMachine exposing (State, Condition, Action, StateMachine,
  addRule, apprise)
import ArrayToList
import Array exposing (Array)
import Random.Array exposing (shuffle)
import Random exposing (Generator)
import Time exposing (Time, inSeconds)
import Text exposing (Text)
import Color exposing (..)
import Collage exposing (Form, text, move)


--- CONSTANTS ---

gridW : Int
gridW = 20
gridH : Int
gridH = 20
spacing : Float
spacing = 30

type KeyColor = R | G | B
type Prop = Key KeyColor | Door KeyColor Bool | Chest

initProps : List Prop
initProps =
 [ Key R, Key G, Key B
 , Door R False, Door G False, Door B False
 , Chest
 ]

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

explorer0 : Explorer
explorer0 =
  { pos = (0.0, 0.0)
  , v = (0.0, 0.0)
  , a = (0.0, 0.0)
  , search = PathFinding.state0
  , state = Resting
  , inv = []
  }

dungeon0 : Dungeon
dungeon0 =
  { floor = Grid.repeat gridW gridH spacing Obstacle
  , loot = []
  , explorer = explorer0
  }

sim0 : Simulation
sim0 = StateMachine.new Array.empty dungeon0

--- BEHAVIOUR ---

isKey : Prop -> Bool
isKey prop = case prop of
  Key _ -> True
  _ -> False

isLockedDoor : Prop -> Bool
isLockedDoor prop = case prop of
  Door _ False -> True
  _ -> False

rules : List (String, Condition Dungeon, Action Dungeon, String)
rules =
 [ ( "Seek Keys"
   , (\dungeon -> dungeon.explorer.state == Resting
       && not (List.any (fst >> isKey) dungeon.loot))
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
   , (\dungeon -> dungeon.explorer.state == Resting
       && not (List.any (fst >> isLockedDoor) dungeon.loot))
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
 , ( "Seek Treasure"
   , (\dungeon -> dungeon.explorer.state == Resting)
   , (\dungeon -> let
       e = dungeon.explorer
       start = screenPointToGrid e.pos dungeon.floor
       chests = List.filter (fst >> (==) Chest) dungeon.loot
       target = case chests of  -- there should only be one...
         (_, p) :: _ -> p
         [] -> (0, 0)
      in { dungeon
       | explorer = { e
         | state = Plotting target
         , search = initSearch start dungeon.floor
         }
       }
     )
   , "Seek Treasure"
   )
 ]

initDungeon : Generator Dungeon
initDungeon = let
  genGrid = Grid.random gridW gridH spacing
  genIndices = shuffle (Array.initialize (gridW * gridH) identity)
 in
  Random.map2 (\grid randIndices -> let
    openIndices = Array.filter
      (\i -> Array.get i grid.array /= Just Obstacle) randIndices
    props = Array.slice 1 (List.length initProps + 1) openIndices
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
  ) genGrid genIndices


--- SIMULATION ---

initSim : Generator Simulation
initSim = Random.map
  (\dungeon -> List.foldl addRule (StateMachine.new states dungeon) rules)
  initDungeon

runDungeon : Time -> Dungeon -> Dungeon
runDungeon t dungeon = let
  dt = inSeconds t
  grid = dungeon.floor
  e = dungeon.explorer
  e_p = (screenPointToGrid e.pos grid)
  node = Grid.get e_p grid
  new_e = explore (e |> stepActor (maxV node) dt) grid
 in
  case List.filter (\(_, p) -> p == e_p) dungeon.loot of
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

simulate : Time -> Simulation -> Simulation
simulate t sim = StateMachine.update (runDungeon t sim.info) sim


--- DRAWING ---

keyColor : KeyColor -> Color
keyColor c = case c of
  R -> red
  G -> green
  B -> blue

drawProp : Prop -> Vec2 -> Form
drawProp prop p = move p <| text <| Text.bold <| Text.height 14 <| case prop of
  Key c -> Text.fromString "K" |> Text.color (keyColor c)
  Door c open -> Text.fromString (if open then "O" else "D")
    |> Text.color (keyColor c)
  Chest -> Text.fromString "T" |> Text.color brown

drawDungeon : Dungeon -> List Form
drawDungeon dungeon = let
  grid = dungeon.floor
  props = dungeon.loot
  e = dungeon.explorer
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

drawSim : Simulation -> List Form
drawSim sim = drawDungeon sim.info
  ++ (case Array.get sim.state states of
      Just s -> [s |> Text.fromString
        |> Text.color purple |> Text.height 18 |> Text.bold
        |> text |> move sim.info.explorer.pos]
      Nothing -> [])
