module Tanks where

import Vec2 exposing (..)
import Color exposing (green)
import Graphics.Collage exposing (Form, filled, rect, solid, rotate)
import ClassicalEngine exposing (Circle, OBR, drawOBR)


--- STRUCTURES ---

type alias Tank = OBR { treads : Vec2 }


--- BEHAVIOURS ---

stepTank : Float -> Tank -> Tank
stepTank dt tank = let
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


--- DRAWING ---

drawTank : Tank -> List Form
drawTank tank = drawOBR (solid green) tank ++ let
  angle = (uncurry (flip atan2) tank.dir)
  barrel_offset = tank.size .*. (0.0, 0.5)
  barrel_pos = (tank.o .+. Vec2.rotate angle barrel_offset)
 in
  [ filled green (uncurry rect (tank.size .*. (0.2, 0.5)))
    |> Graphics.Collage.rotate angle
    |> Graphics.Collage.move barrel_pos
  ]

