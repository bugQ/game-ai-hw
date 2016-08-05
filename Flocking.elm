module Flocking exposing (..)

import Vec2 exposing (..)
import ClassicalEngine exposing (..)
import Random exposing (Generator, generate, initialSeed, Seed)
import Collage exposing (collage)
import Element exposing (Element)
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
  boids : List (Actor {})
}

sim0 : Simulation
sim0 = { params = defaults, boids = [] }

type alias FlockBehavior etc = Float -> Actor etc -> List (Actor etc) -> Vec2


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

canvasW : Int
canvasW = 600
canvasH : Int
canvasH = 400
maxX : Float
maxX = toFloat canvasW / 2
minX : Float
minX = negate maxX
maxY : Float
maxY = toFloat canvasH / 2
minY : Float
minY = negate maxY


--- Behavior ---

-- gets all boids in flock within given radius
neighborhood : Float -> Actor etc -> List (Actor etc) -> List (Actor etc)
neighborhood r boid = List.filter
  (\a -> a.pos /= boid.pos && dist a.pos boid.pos <= r)

-- avoid a neighbor more the closer it is
avoision : Float -> Actor etc -> Actor etc -> Vec2
avoision r boid neighbor = let
  s = boid.pos .-. neighbor.pos
 in
  r *. s ./ sqnorm s

-- steering away from neighbors, inversely related to distance
separation : FlockBehavior etc
separation r boid = neighborhood r boid
  >> List.map (avoision r boid)
  >> List.foldl (.-.) (0, 0)

-- steering toward the average of neighbors' directions
alignment : FlockBehavior etc
alignment r boid = neighborhood r boid
  >> List.foldl (\a dirs -> dirs .+. a.v) (0, 0)
  >> clamp2 0 1

-- steering toward the average of neighbors' positions
cohesion : FlockBehavior etc
cohesion r boid = neighborhood r boid
  >> List.foldl (\a poss -> poss .+. (a.pos .-. boid.pos)) (0, 0)
  >> clamp2 0 1

-- combine the above, weighted
flocking : Float -> Float -> Float -> FlockBehavior etc
flocking a b c r boid flock =
  a *. separation (r / 3) boid flock .+.
  b *. alignment r boid flock .+.
  c *. cohesion (r * 2 / 3) boid flock


--- Simulation ---

initSim : Parameters -> Generator Simulation
initSim params = let
  maxRandV = params.maxV / 4
  genPos = Vec2.random (minX, minY) (maxX, maxY)
  genV = Vec2.random (-maxRandV, -maxRandV) (maxRandV, maxRandV)
 in Random.map2 (\poss vs -> {
   params = params,
   boids = List.map2 (\pos v -> { pos = pos, v = v, a = (0, 0) }) poss vs
 }) (Random.list 60 genPos) (Random.list 60 genV)

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = (inSeconds t)
  {separation, alignment, coherence, neighborhoodRadius} = sim.params
  neighbors = sim.boids
 in { sim | boids = List.map (\boid -> { boid | a =
        flocking separation alignment coherence neighborhoodRadius
          boid sim.boids |> clamp2 0 80
      } |> stepActor 120 dt |> worldWrap ) sim.boids }

worldWrap : Actor etc -> Actor etc
worldWrap boid = { boid | pos = wrap2 (minX, minY) (maxX, maxY) boid.pos }


--- Drawing ---

drawSim : Simulation -> Element
drawSim sim = collage canvasW canvasH <| List.foldl (++) []
  <| List.map (drawBoid grey sim.params.drawVectors) sim.boids
