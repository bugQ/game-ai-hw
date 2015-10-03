module Flocking where

import Vec2 exposing (..)
import ClassicalEngine exposing (..)
import Random exposing (Generator, generate, initialSeed, Seed)
import Graphics.Collage exposing (collage)
import Graphics.Element exposing (Element)
import Time exposing (Time, inSeconds)
import Color exposing (grey)

--- Structures ---

type alias Parameters = {
  maxA : Float,
  maxV : Float,
  separation : Float,
  alignment : Float,
  coherence : Float,
  neighborhoodRadius : Float,
  drawVectors : Bool
}

type alias Simulation = {
  params : Parameters,
  boids : List Actor
}

type alias FlockBehavior = Float -> Actor -> List Actor -> Vec2


--- Constants ---

defaults : Parameters
defaults = {
  maxA = 70,
  maxV = 120,
  separation = 20,
  alignment = 15,
  coherence = 8,
  neighborhoodRadius = 90,
  drawVectors = True
 }

canvasW = 600
canvasH = 400
maxX = canvasW / 2
minX = negate maxX
maxY = canvasH / 2
minY = negate maxY

--- Behavior ---

-- gets all boids in flock within given radius
neighborhood : Float -> Actor -> List Actor -> List Actor
neighborhood r boid = List.filter
  (\a -> a.pos /= boid.pos && dist a.pos boid.pos <= r)

-- avoid a neighbor more the closer it is
avoision : Float -> Actor -> Actor -> Vec2
avoision r boid neighbor = let
  s = boid.pos .-. neighbor.pos
 in
  r *. s ./ sqnorm s

-- steering away from neighbors, inversely related to distance
separation : FlockBehavior
separation r boid = neighborhood r boid
  >> List.map (avoision r boid)
  >> List.foldl (.-.) (0, 0)

-- steering toward the average of neighbors' directions
alignment : FlockBehavior
alignment r boid = neighborhood r boid
  >> List.foldl (\a dirs -> dirs .+. a.v) (0, 0)
  >> clamp2 0 1

-- steering toward the average of neighbors' positions
cohesion : FlockBehavior
cohesion r boid = neighborhood r boid
  >> List.foldl (\a poss -> poss .+. (a.pos .-. boid.pos)) (0, 0)
  >> clamp2 0 1

-- combine the above, weighted
flocking : Float -> Float -> Float -> FlockBehavior
flocking a b c r boid flock =
  a *. separation (r / 3) boid flock .+.
  b *. alignment r boid flock .+.
  c *. cohesion (r / 2) boid flock


--- Simulation ---

initSim : Seed -> Simulation
initSim seed0 = let
  rand1 = Random.pair (Random.float minX maxX) (Random.float minY maxY)
  (poss, seed1) = generate (Random.list 60 rand1) seed0
  rand2 = Random.pair (Random.float -30 30) (Random.float -30 30)
  (vs, seed2) = generate (Random.list 60 rand2) seed1
 in {
   params = defaults,
   boids = List.map2 (\pos v -> { pos = pos, v = v, a = (0, 0) }) poss vs
 }

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = (inSeconds t)
  {separation, alignment, coherence, neighborhoodRadius} = sim.params
  neighbors = sim.boids
 in { sim | boids <- List.map (\boid -> { boid | a <-
        flocking separation alignment coherence neighborhoodRadius
          boid sim.boids |> clamp2 0 80
      } |> stepActor 120 dt |> worldWrap ) sim.boids }

worldWrap : Actor -> Actor
worldWrap boid = { boid | pos <- wrap2 (minX, minY) (maxX, maxY) boid.pos }

--- Drawing ---
drawSim : Simulation -> Element
drawSim sim = collage canvasW canvasH <| List.foldl (++) []
  <| List.map (drawBoid grey sim.params.drawVectors) sim.boids
