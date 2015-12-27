module ClassicalEngine where

import Vec2 exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (Color, charcoal, red, blue)

--- Structures ---

-- a basic kinematic entity
type alias Actor etc = { etc |
  pos : Vec2,
  v : Vec2,
  a : Vec2
}

-- just a circle. origin and radius.
type alias Circle = {
  o : Vec2,
  r : Float
 }

-- Oriented Bounding Rectangle, a rectangle in 2D
type alias OBR = {
  o : Vec2,  -- center point of the rectangle
  dir : Vec2,  -- local x axis; local y is perp to this
  size : Vec2  -- width and height
}


--- Kinematics ---

-- update movement of actor based on velocity and acceleration
-- really basic classical motion, no Verlet or anything here, sorry
stepActor : Float -> Float -> Actor etc -> Actor etc
stepActor maxV dt actor = { actor |
  pos = actor.pos .+. actor.v .* dt,
  v = actor.v .+. actor.a .* dt |> clamp2 0 maxV
 }

-- circular wrapping (one dimension)
wrap : Float -> Float -> Float -> Float
wrap min max x = let len = max - min in
  if x < min then x + len else if x > max then x - len else x

-- toroidal wrapping (two dimensions)
wrap2 : Vec2 -> Vec2 -> Vec2 -> Vec2
wrap2 (xmin, ymin) (xmax, ymax) (x, y) =
  (wrap xmin xmax x, wrap ymin ymax y)


--- Collision ---

-- closest point in rectangle from center to given point
nearestPointOBR : OBR -> Vec2 -> Vec2
nearestPointOBR obr p = let
  d = p .-. obr.o
  -- normals/axes of rectangle
  nx = obr.dir
  ny = perp obr.dir
  -- apothems/extents of rectangle
  rx = fst obr.size / 2
  ry = snd obr.size / 2
 in
  (d `dot` nx |> clamp -rx rx) *. nx .+.
  (d `dot` ny |> clamp -ry ry) *. ny .+. obr.o

-- if they intersect, gives closest point to circle within rect
collideOBRxCircle : OBR -> Circle -> Maybe Vec2
collideOBRxCircle obr circ = let p = nearestPointOBR obr circ.o in
  if sqnorm (p .-. circ.o) > circ.r * circ.r then Nothing else Just p


--- Drawing ---

drawVehicle : Color -> Actor etc -> List Form
drawVehicle color actor = [
  circle 8 |> filled color |> move actor.pos,
  circle 8 |> outlined (solid charcoal) |> move actor.pos,
  traced (solid red) <| segment actor.pos (actor.pos .+. actor.v),
  traced (solid blue) <| segment actor.pos (actor.pos .+. actor.a)
 ]

drawBoid : Color -> Bool -> Actor etc -> List Form
drawBoid color showV actor = let
  vdir = normalize actor.v
  vperp = perp vdir
  boidtriangle = polygon [
    actor.pos .+. 10 *. vdir,
    actor.pos .-. 3 *. vdir .+. 4 *. vperp,
    actor.pos .-. 3 *. vdir .-. 4 *. vperp
  ]
 in [
  boidtriangle |> filled color,
  boidtriangle |> outlined (solid charcoal)
 ] ++ if showV then [
  segment actor.pos (actor.pos .+. actor.a) |> traced (solid blue)
 ] else []

drawObstacle : Color -> Circle -> List Form
drawObstacle color circ = [
  circle circ.r |> filled color |> Graphics.Collage.move circ.o,
  circle circ.r |> outlined (solid charcoal) |> Graphics.Collage.move circ.o
 ]

drawOBR : LineStyle -> OBR -> List Form
drawOBR style obr = [
  outlined style (uncurry rect obr.size) |>
    Graphics.Collage.rotate (uncurry (flip atan2) obr.dir) |>
    Graphics.Collage.move obr.o
 ]
