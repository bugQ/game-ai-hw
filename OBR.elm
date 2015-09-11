module OBR where

import Vec2 exposing (..)
import Graphics.Collage exposing (..)

-- Oriented Bounding Rectangle, a rectangle in 2D
type alias OBR = {  
  center : Vec2,  -- all rectangles have well-defined centers
  direction : Vec2,  -- local x axis; local y is perp to this
  apothems : Vec2  -- also known as extents or half-widths
}

-- closest point in rectangle from center to given point
nearestPoint : OBR -> Vec2 -> Vec2
nearestPoint obr p = let
  d = p .-. obr.center
  -- normals/axes of rectangle
  nx = obr.direction
  ny = perp obr.direction
  -- apothems/extents of rectangle
  rx = fst obr.apothems
  ry = snd obr.apothems
 in
  (d `dot` nx |> clamp -rx rx) *. nx .+.
  (d `dot` ny |> clamp -ry ry) *. ny

drawOBR : LineStyle -> OBR -> List Form
drawOBR style obr = [
  outlined style (uncurry rect (2 *. obr.apothems)) |>
    rotate (uncurry (flip atan2) obr.direction) |>
    move obr.center
 ]