module ChaseEvade exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (Element)
import Random exposing (Generator, generate, initialSeed, Seed)
import Time exposing (..)

import Vec2 exposing (..)
import ClassicalEngine exposing (..)


--- Structures ---

type alias Simulation = {
  quarry : Actor {},
  target : Vec2,
  chaser : Actor {},
  evader : Actor {},
  reset : Float
}


--- Constants ---

maxA : Float
maxA = 50
maxV : Float
maxV = 100


--- Behavior ---

chase : Float -> Float -> Vec2 -> Actor etc -> Actor etc
chase maxV maxA target chaser = { chaser |
  a = normalize (target .-. chaser.pos) .* maxV .-. chaser.v
    |> clamp2 0 maxA }

evade : Float -> Float -> Vec2 -> Actor etc -> Actor etc
evade maxV maxA target evader = { evader |
  a = normalize (target .-. evader.pos) .* -maxV .-. evader.v
    |> clamp2 0 maxA }

arrive : Float -> Float -> Vec2 -> Actor etc -> Actor etc
arrive maxV maxA target arriver = let
  stopt = maxV / maxA
  stopd = stopt * maxV / 2
  diff = target .-. arriver.pos
  d = norm diff
  desired_v = if d < stopd then diff .* (2 / stopt) else diff .* (maxV / d)
 in
  { arriver | a = desired_v .-. arriver.v |> clamp2 0 maxA }


--- Simulation ---

initSim : Generator Simulation
initSim = let
  vec200 = Random.pair (Random.float -200 200) (Random.float -200 200)
  vec40 = Random.pair (Random.float -40 40) (Random.float -40 40)
 in Random.map4 (\r0 r1 r2 r3 ->
    { quarry = {pos = r0 .* 200, v = r1 .* 40, a = (0, 0)}
    , target = r0 .* 200 .+. r1 .* 40
    , chaser = {pos = r2 .* 200, v = (0, 0), a = (0, 0)}
    , evader = {pos = r3 .* 200, v = (0, 0), a = (0, 0)}
    , reset = Time.second * 10
    }
  ) vec200 vec40 vec200 vec200

simulate : Time -> Simulation -> Simulation
simulate t sim = let dt = (inSeconds t) in
  { sim
  | quarry = sim.quarry |> stepActor maxV dt
  , target = sim.quarry.pos .+. sim.quarry.v .*
     (dist sim.target sim.chaser.pos / maxV)
  , chaser = sim.chaser |> chase maxV maxA sim.target |> stepActor maxV dt
  , evader = sim.evader |> evade maxV maxA sim.target |> stepActor maxV dt
  , reset = sim.reset - (inSeconds t)
  }


--- Drawing ---

drawTarget : LineStyle -> Vec2 -> List Form
drawTarget style target =
  [ outlined style (circle 5) |> move target
  , outlined style (circle 8) |> move target
  , traced style <| segment (target .+. (0, 15)) (target .-. (0, 15))
  , traced style <| segment (target .+. (15, 0)) (target .-. (15, 0))
  ]

drawSim : Simulation -> Element
drawSim sim = collage 500 500 (
  drawTarget (solid black) sim.target ++
  drawVehicle grey sim.quarry ++
  drawVehicle red sim.chaser ++
  drawVehicle yellow sim.evader
 )
