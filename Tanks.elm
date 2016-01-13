module Tanks where

import Vec2 exposing (..)
import Color exposing (red, green)
import Graphics.Collage exposing (Form, filled, rect, solid, rotate)
import ClassicalEngine exposing
  (Circle, OBR, wrap2, collideOBRxCircle, drawObstacle, drawOBR)
import Random exposing (Seed, Generator, initialSeed, generate)
import Time exposing (Time)
import Set exposing (Set)


--- CONSTANTS ---

mineSize = 5
tankSize = 20
genTime = 30 * Time.second
moveTime = 0.5 * Time.second
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
 , seed : Seed
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

initSim : Float -> Float -> Seed -> Simulation
initSim w h seed0 = let
  genMines = Random.list 40
    <| Random.map (\pos -> { o = pos, r = mineSize })
    <| Random.pair
        (Random.float (-w*0.5) (w*0.5))
        (Random.float (-h*0.5) (h*0.5))
  (mines, seed) = generate genMines seed0
  tanks = List.repeat 20 tank0
 in
  { size = (w, h)
  , tanks = tanks
  , mines = mines
  , seed = seed
  , reset = genTime
  }

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
    |> Graphics.Collage.rotate angle
    |> Graphics.Collage.move barrel_pos
  ]

drawMine : Circle -> List Form
drawMine = drawObstacle red

drawSim : Simulation -> List Form
drawSim {tanks, mines} =
  List.foldl (drawTank >> (++)) [] tanks
  ++ List.foldl (drawMine >> (++)) [] mines
