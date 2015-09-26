module ChaseEvade where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Random exposing (Generator, generate, initialSeed, Seed)
import Time exposing (..)

import Vec2 exposing (..)
import ClassicalEngine exposing (..)


--- Structures ---

type alias Simulation = {
  quarry : Actor,
  target : Vec2,
  chaser : Actor,
  evader : Actor,
  rand : Generator Vec2,
  seed : Seed,
  reset : Float
}


--- Constants ---

maxA = 50
maxV = 100


--- Behavior ---

chase : Vec2 -> Actor -> Actor
chase target chaser = { chaser |
  a <- normalize (target .-. chaser.pos) .* maxV .-. chaser.v
    |> clamp2 0 maxA }

evade : Vec2 -> Actor -> Actor
evade target evader = { evader |
  a <- normalize (target .-. evader.pos) .* -maxV .-. evader.v
    |> clamp2 0 maxA }


--- Simulation ---

initSim : Seed -> Simulation
initSim seed0 = let
  rand = Random.pair (Random.float -1 1) (Random.float -1 1)
  (r0, seed1) = generate rand seed0
  (r1, seed2) = generate rand seed1
  (r2, seed3) = generate rand seed2
  (r3, seed4) = generate rand seed3
 in {
   quarry = {pos = r0 .* 200, v = r1 .* 40, a = (0, 0)},
   target = r0 .* 200 .+. r1 .* 40,
   chaser = {pos = r2 .* 200, v = (0, 0), a = (0, 0)},
   evader = {pos = r3 .* 200, v = (0, 0), a = (0, 0)},
   rand = rand,
   seed = seed4,
   reset = 0
  }

simulate : Time -> Simulation -> Simulation
simulate t sim = if sim.reset > 10 then initSim sim.rand sim.seed else
  let dt = (inSeconds t) in
  { sim |
   quarry <- sim.quarry |> stepActor maxV dt,
   target <- sim.quarry.pos .+. sim.quarry.v .*
     (dist sim.target sim.chaser.pos / maxV),
   chaser <- sim.chaser |> chase sim.target |> stepActor maxV dt,
   evader <- sim.evader |> evade sim.target |> stepActor maxV dt,
   reset <- sim.reset + (inSeconds t)
  }


--- Drawing ---

drawTarget : LineStyle -> Vec2 -> List Form
drawTarget style target = [
  outlined style (circle 5) |> move target,
  outlined style (circle 8) |> move target,
  traced style <| segment (target .+. (0, 15)) (target .-. (0, 15)),
  traced style <| segment (target .+. (15, 0)) (target .-. (15, 0))
 ]

drawSim : Simulation -> Element
drawSim sim = collage 500 500 (
  drawTarget (solid black) sim.target ++
  drawVehicle grey sim.quarry ++
  drawVehicle red sim.chaser ++
  drawVehicle yellow sim.evader
 )
