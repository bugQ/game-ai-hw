module Tanks exposing (Tank, Simulation,
  sim0, genSim, stepTank, simulate, drawMine, drawSim)

import Vec2 exposing (..)
import Color exposing (red, green)
import Collage exposing (Form, filled, rect, solid, rotate)
import ClassicalEngine exposing
  (Circle, OBR, wrap2, collideOBRxCircle, drawObstacle, drawOBR)
import Random exposing (Generator)
import Time exposing (Time)
import Set exposing (Set)


--- CONSTANTS ---

mineSize : Float
mineSize = 5
tankSize : Float
tankSize = 20
numMines : Int
numMines = 40
numTanks : Int
numTanks = 16
genTime : Time
genTime = 30 * Time.second
moveTime : Time
moveTime = 0.5 * Time.second
treadMax : Float
treadMax = 70


--- STRUCTURES ---

type alias Tank = OBR
 { treads : Vec2
 , inv : Set Vec2
 , moves : List Vec2
 , next : Time
 }

type alias Simulation =
 { size : Vec2
 , tanks : List Tank
 , mines : List Circle
 , reset : Time
 }


--- SIMULATION ---

tank0 : Tank
tank0 =
 { o = (0, 0)
 , dir = (1, 0)
 , size = (tankSize, tankSize)
 , treads = (0, 0)
 , inv = Set.empty
 , moves = []
 , next = moveTime
 }

sim0 : Simulation
sim0 =
 { size = (0, 0)
 , tanks = []
 , mines = []
 , reset = Time.hour
 }

genSim : Float -> Float -> Generator Simulation
genSim w h = let
  genMines = Random.list numMines
    <| Random.map (\pos -> { o = pos, r = mineSize })
    <| Random.pair
        (Random.float (-w*0.5) (w*0.5))
        (Random.float (-h*0.5) (h*0.5))
  genTanks = Random.list (numTanks - 1)
    <| Random.map (\moves -> { tank0 | moves = moves })
    <| Random.list (genTime / moveTime |> ceiling)
    <| Random.pair
        (Random.float -1.0 1.0)
        (Random.float -1.0 1.0)
 in
  Random.map2 (\mines tanks ->
      { size = (w, h)
      , tanks = tank0 :: tanks
      , mines = mines
      , reset = genTime
      }
    ) genMines genTanks

{-
initSim : Float -> Float -> Seed -> Simulation
initSim w h seed0 = let
  genMines = Random.list numMines
    <| Random.map (\pos -> { o = pos, r = mineSize })
    <| Random.pair
        (Random.float (-w*0.5) (w*0.5))
        (Random.float (-h*0.5) (h*0.5))
  (mines, seed1) = generate genMines seed0
  genTanks = Random.list (numTanks - 1)
    <| Random.map (\moves -> { tank0 | moves = moves })
    <| Random.list (genTime / moveTime |> ceiling)
    <| Random.pair
        (Random.float -1.0 1.0)
        (Random.float -1.0 1.0)
  (tanks, seed2) = generate genTanks seed1
 in
  { size = (w, h)
  , tanks = tank0 :: tanks
  , mines = mines
  , seed = seed2
  , reset = genTime
  }
-}

simulate : Time -> Simulation -> Simulation
simulate tick sim =
  { sim | tanks = List.map ((stepTank tick)
      >> (\tank -> { tank | o =
          wrap2 (sim.size .* -0.5) (sim.size .* 0.5) tank.o })
      >> (\tank -> { tank | inv =
          List.foldl (\mine inv -> case collideOBRxCircle tank mine of
              Just _ -> Set.insert mine.o inv
              Nothing -> inv
            ) tank.inv sim.mines
        })
    ) sim.tanks
  }


--- BEHAVIOURS ---

stepTank : Time -> Tank -> Tank
stepTank tick tank = let
  nextMove = tank.next - tick
  tank = if nextMove > 0 then
      { tank | next = nextMove }
    else case tank.moves of
      move :: moves ->
        { tank | treads = move, moves = moves, next = nextMove + moveTime }
      [] -> { tank | next = 0 }
  dt = Time.inSeconds tick
  (treadL, treadR) = clamp2 -1 1 tank.treads .* treadMax
  width = fst tank.size
 in
  if treadL == treadR then
    if treadL == 0.0 then
      tank
    else
      { tank | o = perp tank.dir .* (treadR * dt) .+. tank.o }
  else if treadL == -treadR then
    { tank | dir = Vec2.rotate ((treadR - treadL) / width * dt) tank.dir }
  else let
    r = width / (1 - treadL / treadR) - width * 0.5
    pivot = tank.o .-. (r *. tank.dir)
    v = (treadL + treadR) * 0.5
    new_dir = Vec2.rotate (v / r * dt) tank.dir
    new_pos = pivot .+. (new_dir .* r)
   in
    { tank | o = new_pos, dir = new_dir }


--- DRAWING ---

drawTank : Tank -> List Form
drawTank tank = drawOBR (solid green) tank ++ let
  angle = (atan2 (snd tank.dir) (fst tank.dir))
  barrel_offset = tank.size .*. (0.0, 0.5)
  barrel_pos = (tank.o .+. Vec2.rotate angle barrel_offset)
 in
  [ filled green (uncurry rect (tank.size .*. (0.2, 0.5)))
    |> Collage.rotate angle
    |> Collage.move barrel_pos
  ]

drawMine : Circle -> List Form
drawMine = drawObstacle red

drawSim : Simulation -> List Form
drawSim {tanks, mines} =
  List.foldl (drawTank >> (++)) [] tanks
  ++ List.foldl (drawMine >> (++)) [] mines
