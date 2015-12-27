module Tanks where

import Vec2 exposing (..)
import Graphics.Collage exposing (LineStyle)
import ClassicalEngine exposing (OBR)


--- BEHAVIOURS ---

stepTank : Float -> Vec2 -> OBR -> OBR
stepTank dt (treadL, treadR) tank =
  if treadL == treadR then
    if treadL == 0.0 then
      tank
    else
      { tank | o = perp tank.dir .* (treadR * dt) .+. tank.o }
  else let
    width = fst tank.size
    r = width / (1 - treadL / treadR) - width / 2.0
    pivot = tank.o .-. (r *. tank.dir)
    v = (treadL + treadR) / 2.0
    new_dir = rotate (v / r * dt) tank.dir
    new_pos = pivot .+. (new_dir .* r)
   in
    { tank | o = new_pos, dir = new_dir }


--- DRAWING ---

drawTank : OBR -> List Form
drawTank tank = drawOBR (solid green) tank ++
  [ filled green (uncurry rect (tank.size .*. (0.2, 0.5))) |>
    rotate (uncurry (flip atan2) tank.dir) |>
    move (tank.o .+. tank.size .*. (0.0, 0.5))
  ]

