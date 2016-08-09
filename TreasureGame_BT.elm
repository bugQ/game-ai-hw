module TreasureGame_BT exposing (..)

import Grid exposing (screen2grid)
import BehaviourTree exposing (..)
import TreasureGame exposing (Explorer, Dungeon, dungeon0, Prop(Chest),
  isKey, isLockedDoor, initDungeon, runDungeon, drawDungeon)
import PathFollowing exposing (Exploration(..), explore)
import Collage exposing (Form, text, move)
import Text
import Time exposing (Time, inSeconds)
import Random exposing (Generator)
import Color exposing (purple)


--- STRUCTURES ---

type alias Simulation = (Dungeon, Behaviour Dungeon)

sim0 : Simulation
sim0 = (dungeon0, BehaviourTree.empty)

--- BEHAVIOR ---

seekKey : Routine Dungeon
seekKey dungeon = let
  e = dungeon.explorer
  grid = dungeon.floor
  start = screen2grid e.pos dungeon.floor
  keys = List.filter (fst >> isKey) dungeon.loot
 in
  case keys of
    [] -> ({ dungeon | explorer = { e | state = Resting } }, Failure)
    (_, target) :: _ -> case dungeon.explorer.state of
      Resting ->
        ({ dungeon | explorer = { e | state = Plotting target } }, Success)
      _ -> (dungeon, Running)

seekDoor : Routine Dungeon
seekDoor dungeon = let
  e = dungeon.explorer
  grid = dungeon.floor
  start = screen2grid e.pos dungeon.floor
  lockedDoors = List.filter (fst >> isLockedDoor) dungeon.loot
 in
  case lockedDoors of
    [] -> ({ dungeon | explorer = { e | state = Resting } }, Failure)
    (_, target) :: _ -> case dungeon.explorer.state of
      Resting ->
        ({ dungeon | explorer = { e | state = Plotting target } }, Success)
      _ -> (dungeon, Running)

seekTreasure : Routine Dungeon
seekTreasure dungeon = let
  e = dungeon.explorer
  grid = dungeon.floor
  start = screen2grid e.pos dungeon.floor
  chests = List.filter (fst >> (==) Chest) dungeon.loot
 in
  case chests of
    (_, target) :: _ -> case dungeon.explorer.state of
      Resting ->
        ({ dungeon | explorer = { e | state = Plotting target } }, Running)
      _ -> (dungeon, Running)
    [] -> (dungeon, Success)

initTree : Behaviour Dungeon
initTree = Sequence
 [ repeat (Leaf seekKey)
 , repeat (Leaf seekDoor)
 , Leaf seekTreasure
 ]

--- SIMULATION ---

initSim : Generator Simulation
initSim = Random.map (\dungeon -> (dungeon, initTree)) initDungeon

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
