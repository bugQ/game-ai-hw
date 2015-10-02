module Flocking where

import Vec2 exposing (..)
import ClassicalEngine exposing (..)
import Array

type alias FlockBehavior = Actor -> List Actor -> Vec2

--- Behavior ---

-- gets all boids in flock within given radius
neighborhood : Array Actor -> Float -> Actor -> List Actor
neighborhood flock radius boid =
  Array.filter (\a -> dist a.pos boid.pos <= radius) flock

-- avoid a neighbor more the closer it is
avoision : Actor -> Actor -> Vec2
avoision boid neighbor = let
  s = neighbor.pos .-. boid.pos
 in
  s ./ sqnorm s

-- steering away from neighbors, closest most
separation : Actor -> List Actor -> Vec2
separation boid = map (avoision boid) >> foldr (+) 0
