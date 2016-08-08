module MonsterGame exposing (..)

import TreasureGame exposing (Prop(Chest), Dungeon, Explorer,
  explorer0, dungeon0, initDungeon, runDungeon, drawDungeon)
import TreasureGame_BT exposing (Simulation, sim0, initTree)
import ChaseEvade exposing (chase, evade)
import ClassicalEngine exposing (Actor, drawVehicle)
import PathFinding exposing (AStarState, state0, aStar, heatMap, drawPath, inf)
import PathFollowing exposing (Exploration(..), maxA, maxV)
import BehaviourTree exposing (Behaviour)
import Grid exposing (Grid, Point,
  manhattan, neighbors4, neighbors20, screen2grid, grid2screen)
import Vec2 exposing (..)
import Random exposing (Generator)
import Time exposing (Time)
import Collage exposing (Form)
import Color exposing (orange, lightGreen, darkOrange)
import Text
import Array exposing (Array)
import Array.Extra as Array

--- STRUCTURES ---

type alias Game =
  { dungeon : Dungeon
  , script : Behaviour Dungeon
  , score : Int
  , reset : Time
  , goalHeatMap : Array Float
  , monsterHeatMap : Array Float
  , prevMonPoint : Point
  }

game0 : Game
game0 =
  { dungeon = dungeon0
  , script = BehaviourTree.empty
  , score = 0
  , reset = resetTime
  , goalHeatMap = Array.empty
  , monsterHeatMap = Array.empty
  , prevMonPoint = (0, 0)
  }


--- CONSTANTS ---

resetTime : Time
resetTime = Time.second * 5

goalHeat : Float
goalHeat = 400
monsterHeat : Float
monsterHeat = 200


--- BEHAVIOR ---

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
          |> Maybe.map (\c -> (c + toFloat (manhattan p localGoal), p))
      )
    |> List.maximum |> Maybe.withDefault (0, point)
 in
  chase (maxV (Grid.get point grid)) maxA (grid2screen target grid) actor
  --List.foldl (.+.) (0, 0) |> (\v -> if (v == (0,0)) then v else normalize v)


--- SIMULATION ---

genGame : Generator Game
genGame =
  Random.map (\d -> { game0 | dungeon = d, script = initTree,
      prevMonPoint = screen2grid d.monster.pos d.floor }) initDungeon

controlMonster : Vec2 -> Game -> Game
controlMonster pointer game = let
  {dungeon} = game
  {floor, monster} = dungeon
  start = screen2grid monster.pos floor
  goal = screen2grid pointer floor
 in
  { game | dungeon = { dungeon | monster =
    { monster | state = Seeking (aStar floor start goal) } } }

run : Time -> Game -> Game
run t game = if game.score /= 0 then { game | reset = game.reset - t } else
 let
  (new_dungeon, _, new_script) =
    BehaviourTree.step game.script (runDungeon t game.dungeon)
  {floor, monster, explorer} = new_dungeon
  eMaxV = maxV (Grid.get (screen2grid explorer.pos floor) floor)
  mp = screen2grid monster.pos floor
  goalHeatMap = case explorer.state of
    Plotting goal -> heatMap floor goalHeat goal
    _ -> game.goalHeatMap
  monsterHeatMap = if mp == game.prevMonPoint then game.monsterHeatMap else
    heatMap floor monsterHeat mp
  new_e = explorer |> heatSeek floor (goalHeatMap)
  new_game = { game
    | dungeon = { new_dungeon | explorer = new_e }
    , script = new_script
    , goalHeatMap = goalHeatMap
    , monsterHeatMap = monsterHeatMap
    }
 in
  if dist monster.pos explorer.pos < 16
    then { new_game | score = -1 }
  else if List.all (fst >> (/=) Chest) new_dungeon.loot
    then { new_game | score = 1 }
  else new_game


--- DRAWING ---

drawGame : Game -> List Form
drawGame {dungeon, score, goalHeatMap} = drawDungeon dungeon
  ++ drawVehicle orange dungeon.monster
  ++ case dungeon.monster.state of
    Seeking path -> [drawPath path dungeon.floor]
    _ -> []
  ++ [Collage.text <| Text.bold <| Text.height 32 <| case score of
        1 -> Text.fromString "EXPLORER WINS" |> Text.color lightGreen
        (-1) -> Text.fromString "MONSTER WINS" |> Text.color darkOrange
        _ -> Text.fromString ""]
  ++ PathFinding.drawRunningCosts goalHeatMap dungeon.floor
