module OBR where

import Vec2 exposing (..)
import Graphics.Collage exposing (..)

-- Oriented Bounding Rectangle, a rectangle in 2D
type alias OBR = {  
  o : Vec2,  -- center point of the rectangle
  dir : Vec2,  -- local x axis; local y is perp to this
  size : Vec2  -- width and height
}

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

drawOBR : LineStyle -> OBR -> List Form
drawOBR style obr = [
  outlined style (uncurry rect obr.size) |>
    rotate (uncurry (flip atan2) obr.dir) |>
    move obr.o
 ]