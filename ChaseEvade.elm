module ChaseEvade where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Random exposing (Generator, generate, initialSeed, Seed)
import Time exposing (..)
import Vec2 exposing (..)

type alias Actor = {
  pos : Vec2,
  v : Vec2,
  a : Vec2
}

maxA = 50
maxV = 100

chase : Vec2 -> Actor -> Actor
chase target chaser = { chaser |
  a <- normalize (target .-. chaser.pos) .* maxV .-. chaser.v
    |> clamp2 0 maxA }

evade : Vec2 -> Actor -> Actor
evade target evader = { evader |
  a <- normalize (target .-. evader.pos) .* -maxV .-. evader.v
    |> clamp2 0 maxA }

physics : Float -> Actor -> Actor
physics dt actor = { actor |
  pos <- actor.pos .+. actor.v .* dt,
  v <- actor.v .+. actor.a .* dt |> clamp2 0 maxV
 }
 

type alias Simulation = {
  quarry : Actor,
  target : Vec2,
  chaser : Actor,
  evader : Actor,
  rand : Generator Vec2,
  seed : Seed,
  reset : Float
}

simulate : Time -> Simulation -> Simulation
simulate t sim = if sim.reset > 10 then
  let
   (r0, seed1) = generate sim.rand sim.seed
   (r1, seed2) = generate sim.rand seed1
   (r2, seed3) = generate sim.rand seed2
   (r3, seed4) = generate sim.rand seed3
  in
  { sim |
   quarry <- {pos = r0 .* 200, v = r1 .* 40, a = (0, 0)},
   target <- r0 .* 200 .+. r1 .* 40,
   chaser <- {pos = r2 .* 200, v = (0, 0), a = (0, 0)},
   evader <- {pos = r3 .* 200, v = (0, 0), a = (0, 0)},
   reset <- 0,
   seed <- seed4
  }
 else
  let dt = (inSeconds t) in
  { sim |
   quarry <- sim.quarry |> physics dt,
   target <- sim.quarry.pos .+. sim.quarry.v .*
     (dist sim.target sim.chaser.pos / maxV),
   chaser <- sim.chaser |> chase sim.target |> physics dt,
   evader <- sim.evader |> evade sim.target |> physics dt,
   reset <- sim.reset + (inSeconds t)
  }

drawActor : Color -> Actor -> List Form
drawActor color actor = [
  filled color (circle 10) |> move actor.pos,
  outlined (solid charcoal) (circle 10) |> move actor.pos, 
  traced (solid red) <| segment actor.pos (actor.pos .+. actor.v),
  traced (solid blue) <| segment actor.pos (actor.pos .+. actor.a)
 ]

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
  drawActor grey sim.quarry ++
  drawActor red sim.chaser ++
  drawActor yellow sim.evader
 )
