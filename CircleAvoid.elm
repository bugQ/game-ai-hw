module CircleAvoid where

import List exposing (map, foldr)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Vec2 exposing (..)
import OBR exposing (OBR, drawOBR)
import ChaseEvade exposing (Actor, drawActor, physics, maxV, maxA)

type alias Circle = {
  o : Vec2,
  r : Float
 }

-- whether they intersect, plus closest point on rect
circlexOBR : Circle -> OBR -> Maybe Vec2
circlexOBR circ obr = let p = OBR.nearestPoint obr circ.o in
  if sqnorm (p .-. circ.o) > circ.r * circ.r then Nothing else Just p

terrain : List Circle
terrain = [
  {o = (-200, -200), r = 50},
  {o = (200, -200), r = 50},
  {o = (-200, 200), r = 50},
  {o = (200, 200), r = 50},
  {o = (50, -70), r = 80},
  {o = (-120, 120), r = 25},
  {o = (-95, 25), r = 40},
  {o = (12, 107), r = 50},
  {o = (123, 55), r = 30},
  {o = (-125, -75), r = 30},
  {o = (-75, -145), r = 13},
  {o = (123, 130), r = 13}
 ]

drawObstacle : Color -> Circle -> List Form
drawObstacle color circ = [
  move circ.o <| filled color <| circle circ.r,
  move circ.o <| outlined (solid charcoal) <| circle circ.r
 ]

futureProj : Actor -> OBR
futureProj actor = let
  sqv = sqnorm actor.v
  nx = actor.v ./ sqrt sqv
  rf = sqv / (4 * maxA)
  r = rf *. nx
 in
  { center = actor.pos .+. r, direction = nx, apothems = (rf, 10) }

initActor : Actor
initActor = { pos = (0, 10), v = (maxV, 0), a = (-15, 15) }

main = collage 400 400 <|
  (foldr (++) (drawActor green initActor) <|
   map (drawObstacle grey) terrain) ++
  drawOBR (solid yellow) (futureProj initActor)