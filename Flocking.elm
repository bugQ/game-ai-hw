module Flocking where

import Vec2 exposing (..)
import ClassicalEngine exposing (..)
import Random exposing (Generator, generate, initialSeed, Seed)
import Graphics.Collage exposing (collage)
import Graphics.Element exposing (Element)
import Time exposing (Time, inSeconds)
import Color exposing (grey)

--- Structures ---

type alias Simulation = {
  boids : List Actor
}

type alias FlockBehavior = Actor -> List Actor -> Vec2


--- Behavior ---

-- gets all boids in flock within given radius
neighborhood : List Actor -> Float -> Actor -> List Actor
neighborhood flock radius boid =
  List.filter (\a -> dist a.pos boid.pos <= radius) flock

-- avoid a neighbor more the closer it is
avoision : Actor -> Actor -> Vec2
avoision boid neighbor = let
  s = neighbor.pos .-. boid.pos
 in
  s ./ sqnorm s

-- steering away from neighbors, inversely related to distance
separation : FlockBehavior
separation boid = List.map (avoision boid) >> List.foldl (.+.) (0,0)

-- steering toward the average of neighbors' directions
alignment : FlockBehavior
alignment boid = List.foldl (\a (dirs, n) -> (dirs .+. normalize a.v, n+1))
  ((0,0),0) >> uncurry (./)

-- steering toward the average of neighbors' positions
cohesion : FlockBehavior
cohesion boid = List.foldl (\a (poss, n) -> (poss .+. a.pos, n+1))
  ((0,0),0) >> uncurry (./) >> (flip (.-.)) boid.pos

-- combine the above, weighted
flocking : Float -> Float -> Float -> FlockBehavior
flocking a b c boid boids = [(a, separation), (b, alignment), (c, cohesion)]
  |> List.foldl (\(coef, beh) sum -> sum .+. coef *. beh boid boids) (0,0)


--- Simulation ---

initSim : Seed -> Simulation
initSim seed0 = let
  rand1 = Random.pair (Random.float -200 200) (Random.float -200 200)
  (poss, seed1) = generate (Random.list 50 rand1) seed0
  rand2 = Random.pair (Random.float -30 30) (Random.float -30 30)
  (vs, seed2) = generate (Random.list 50 rand2) seed1
 in
  { boids = List.map2 (\pos v -> { pos = pos, v = v, a = (0,0) }) poss vs }

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = (inSeconds t)
 in { sim |
  boids <- List.map (\boid ->
      { boid | a <- flocking 1 2 3 boid sim.boids } -- |> stepActor 50 dt
    ) sim.boids
 }


--- Drawing ---
drawSim : Simulation -> Element
drawSim sim = collage 400 400 <|
  List.foldl (++) [] <| List.map (drawBoid grey) sim.boids
