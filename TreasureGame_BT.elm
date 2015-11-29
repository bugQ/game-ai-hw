module TreasureGame_BT where

import Grid exposing (screenPointToGrid)
import PathFinding exposing (initSearch)
import BehaviourTree exposing (..)
import TreasureGame exposing (Explorer, Dungeon, Prop(Chest),
  isKey, isLockedDoor, initDungeon, runDungeon, drawDungeon)
import PathFollowing exposing (Exploration(Plotting, Arriving, Resting), explore)
import Graphics.Collage exposing (Form, text, move)
import Text
import Time exposing (Time, inSeconds)
import Random exposing (Seed)
import Color exposing (purple)

--- STRUCTURES ---

type alias Simulation = (Dungeon, Behaviour Dungeon)


--- BEHAVIOR ---

seekKey : Routine Dungeon
seekKey dungeon = let
  e = dungeon.explorer
  grid = dungeon.floor
  start = screenPointToGrid e.pos dungeon.floor
  keys = List.filter (fst >> isKey) dungeon.loot
 in
  case keys of
    (_, target) :: _ -> case dungeon.explorer.state of
      Resting -> ({ dungeon | explorer =
        { e | search = initSearch start grid, state = Plotting target } }
       , Success)
      _ -> (dungeon, Running)
    [] -> (dungeon, Failure)

seekDoor : Routine Dungeon
seekDoor dungeon = let
  e = dungeon.explorer
  grid = dungeon.floor
  start = screenPointToGrid e.pos dungeon.floor
  lockedDoors = List.filter (fst >> isLockedDoor) dungeon.loot
 in
  case lockedDoors of
    (_, target) :: _ -> case dungeon.explorer.state of
      Resting -> ({ dungeon | explorer =
        { e | search = initSearch start grid, state = Plotting target } }
       , Success)
      _ -> (dungeon, Running)
    [] -> (dungeon, Failure)

seekTreasure : Routine Dungeon
seekTreasure dungeon = let
  e = dungeon.explorer
  grid = dungeon.floor
  start = screenPointToGrid e.pos dungeon.floor
  chests = List.filter (fst >> (==) Chest) dungeon.loot
 in
  case chests of
    (_, target) :: _ -> case dungeon.explorer.state of
      Resting -> ({ dungeon | explorer =
        { e | search = initSearch start grid, state = Plotting target } }
       , Success)
      _ -> (dungeon, Running)
    [] -> (dungeon, Success)

initTree : Behaviour Dungeon
initTree = Sequence
 [ repeat (Leaf seekKey)
 , repeat (Leaf seekDoor)
 , Leaf seekTreasure
 ]


--- SIMULATION ---

initSim : Seed -> Simulation
initSim seed = (initDungeon seed, initTree)

simulate : Time -> Simulation -> Simulation
simulate t (dungeon, tree) = let
  (new_dungeon, _, new_tree) = BehaviourTree.step tree (runDungeon t dungeon)
 in
  (new_dungeon, new_tree)


--- DRAWING ---

stateName : Behaviour Dungeon -> String
stateName tree = case tree of
  Sequence bb -> case List.length bb of
    3 -> "Seek Keys"
    2 -> "Seek Doors"
    1 -> "Seek Treasure"
    0 -> "Celebrate !!"
    _ -> ""
  _ -> ""

drawSim : Simulation -> List Form
drawSim (dungeon, tree) = drawDungeon dungeon
  ++ [stateName tree |> Text.fromString
        |> Text.color purple |> Text.height 18 |> Text.bold
        |> text |> move dungeon.explorer.pos]
