module Tanks where

import Vec2 exposing (..)
import Color exposing (red, green)
import Graphics.Collage exposing (Form, filled, rect, solid, rotate)
import ClassicalEngine exposing
  (Circle, OBR, collideOBRxCircle, drawObstacle, drawOBR)
import Random exposing (Seed, Generator, initialSeed, generate)
import Time exposing (Time)


--- CONSTANTS ---

mineSize = 5

--- STRUCTURES ---

type alias Tank = OBR { treads : Vec2 }

type alias Simulation =
 { tanks : List Tank
 , mines : List Circle
 , rand : Generator (List Circle)
 , seed : Seed
 , reset : Time
 }

--- SIMULATION ---

tank0 : Tank
tank0 =
 { o = (0, 0)
 , dir = (1, 0)
 , size = (20, 20)
 , treads = (0, 0)
 }

initSim : Float -> Float -> Seed -> Simulation
initSim w h seed0 = let
  rand = Random.list 40
    <| Random.map (\pos -> { o = pos, r = mineSize })
    <| Random.pair
        (Random.float (-w*0.5) (w*0.5))
        (Random.float (-h*0.5) (h*0.5))
  (mines, seed) = generate rand seed0
  tanks = List.repeat 20 tank0
 in
  { tanks = tanks
  , mines = mines
  , rand = rand
  , seed = seed
  , reset = 20 * Time.second
  }

--- BEHAVIOURS ---

stepTank : Time -> Tank -> Tank
stepTank tick tank = let
  dt = Time.inSeconds tick
  (treadL, treadR) = tank.treads
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

simulate : Time -> Simulation -> Simulation
simulate tick sim = let
  moved = { sim | tanks = List.map (stepTank tick) sim.tanks }
 in
  { moved | mines =
     List.filter
      (\mine -> List.all (\tank -> collideOBRxCircle tank mine == Nothing) sim.tanks)
      sim.mines
  }


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
