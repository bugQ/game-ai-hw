module MonsterGame exposing (..)

import TreasureGame exposing (Prop(Chest), Dungeon, Explorer,
  explorer0, dungeon0, initDungeon, runDungeon, drawDungeon)
import TreasureGame_BT exposing (Simulation, sim0, initTree)
import ChaseEvade exposing (chase, evade)
import ClassicalEngine exposing (Actor, drawVehicle)
import PathFinding exposing (AStarState,
  state0, initSearch, aStar, drawPath, inf)
import PathFollowing exposing (Exploration(..), maxA, maxV)
import BehaviourTree exposing (Behaviour)
import HeatMap exposing (heatNav, heatProx)
import Grid exposing (Grid, Point,
  manhattan, neighbors4, neighbors20, screen2grid, grid2screen)
import Vec2 exposing (..)
import Random exposing (Generator)
import Time exposing (Time)
import Collage exposing (Form)
import Color exposing (orange, lightGreen, darkOrange)
import Text
import Array exposing (Array)


--- STRUCTURES ---

type alias Game =
  { dungeon : Dungeon
  , script : Behaviour Dungeon
  , score : Int
  , reset : Time
  , goalMap : Array Float
  , monsterMap : Array Float
  , prevMonPoint : Point
  }

game0 : Game
game0 =
  { dungeon = dungeon0
  , script = BehaviourTree.empty
  , score = 0
  , reset = resetTime
  , goalMap = Array.empty
  , monsterMap = Array.empty
  , prevMonPoint = (0, 0)
  }


--- CONSTANTS ---

resetTime : Time
resetTime = Time.second * 5

goalHeat : Float
goalHeat = 1
monsterHeat : Float
monsterHeat = 1


--- BEHAVIOR ---

{-
heatSeek : Grid -> Array Float -> Actor etc -> Actor etc
heatSeek grid heatMap actor = let
  (x, y) = actor.pos
  point = screen2grid (clamp -300 300 x, clamp -300 300 y) grid
  filterer : (Grid.Point, Float) -> Maybe (Float, Grid.Point)
  filterer = ( \(p, _) ->
      Array.get (Grid.index p grid) heatMap
        |> Maybe.map (\c -> (c, p))
    )
  (_, localGoal) = neighbors20 point grid
    |> List.filterMap filterer
    |> List.maximum |> Maybe.withDefault (0, point)
  (_, target) = neighbors4 point grid
    |> List.filterMap ( \(p, _) ->
        Array.get (Grid.index p grid) heatMap
          |> Maybe.map (\c -> (c - toFloat (manhattan p localGoal), p))
      )
    |> List.maximum |> Maybe.withDefault (0, point)
 in
  chase (maxV (Grid.get point grid)) maxA (grid2screen target grid) actor
  --List.foldl (.+.) (0, 0) |> (\v -> if (v == (0,0)) then v else normalize v)
-}

heatSeek : Grid -> Array Float -> Array Float -> Actor etc -> Actor etc
heatSeek grid heatMap dangerMap actor = let
  (x, y) = actor.pos
  p = screen2grid (clamp -300 300 x, clamp -300 300 y) grid
  hotNeighbors = neighbors20 p grid
    |> List.filterMap ( \(point, dist) -> let i = Grid.index point grid in
        Array.get i heatMap `Maybe.andThen` (\good ->
          Array.get i dangerMap `Maybe.andThen` (\bad ->
            Just (good - bad, point, dist)
      )))
  (_, localGoal, _) = List.maximum hotNeighbors |> Maybe.withDefault (0, p, 0)
{-(_, target) = hotNeighbors
  |> List.filterMap ( \(good, point, dist) -> if dist > 1 then Nothing else
        Just (good - toFloat (manhattan p localGoal), point) )
  |> List.maximum |> Maybe.withDefault (0, p)
-}
  (_, target) = hotNeighbors
    |> List.filterMap ( \(good, point, _) -> case manhattan point p of
        1 -> Just (good - toFloat (manhattan point localGoal), point)
        _ -> Nothing
      )
    |> List.maximum |> Maybe.withDefault (0, p)
 in
  chase (maxV (Grid.get p grid)) maxA (grid2screen target grid) actor
  --List.foldl (.+.) (0, 0) |> (\v -> if (v == (0,0)) then v else normalize v)


--- SIMULATION ---

genGame : Generator Game
genGame =
  Random.map (\d -> { game0 | dungeon = d, script = initTree,
      prevMonPoint = (-1, -1) }) initDungeon

controlMonster : Vec2 -> Game -> Game
controlMonster pointer game = let
  {dungeon} = game
  {floor, monster} = dungeon
  start = screen2grid monster.pos floor
  goal = screen2grid pointer floor
 in
  { game | dungeon = { dungeon | monster = { monster | state = Plotting goal }}}

run : Time -> Game -> Game
run t game = if game.score /= 0 then { game | reset = game.reset - t } else
 let
  (new_dungeon, _, new_script) =
    BehaviourTree.step game.script (runDungeon t game.dungeon)
  {floor, monster, explorer} = new_dungeon
  eMaxV = maxV (Grid.get (screen2grid explorer.pos floor) floor)
  mp = screen2grid monster.pos floor
  goalMap = case explorer.state of
    Plotting goal -> heatNav floor goalHeat goal
    _ -> game.goalMap
  monsterMap = if mp == game.prevMonPoint
      then game.monsterMap
    else heatProx floor monsterHeat mp
  new_e = heatSeek floor goalMap monsterMap <| case explorer.state of
    Plotting goal -> { explorer | state = Arriving goal }
    _ -> explorer
  new_game = { game
    | dungeon = { new_dungeon | explorer = new_e }
    , script = new_script
    , goalMap = goalMap
    , monsterMap = monsterMap
    , prevMonPoint = mp
    }
 in
  if dist monster.pos explorer.pos < 16
    then { new_game | score = -1 }
  else if List.all (fst >> (/=) Chest) new_dungeon.loot
    then { new_game | score = 1 }
  else new_game


--- DRAWING ---

drawGame : Game -> List Form
drawGame {dungeon, score, goalMap} = drawDungeon dungeon
  ++ drawVehicle orange dungeon.monster
  ++ case dungeon.monster.state of
    Seeking path -> [drawPath path dungeon.floor]
    _ -> []
  ++ [Collage.text <| Text.bold <| Text.height 32 <| case score of
        1 -> Text.fromString "EXPLORER WINS" |> Text.color lightGreen
        (-1) -> Text.fromString "MONSTER WINS" |> Text.color darkOrange
        _ -> Text.fromString ""]
