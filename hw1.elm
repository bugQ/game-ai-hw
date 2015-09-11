import ChaseEvade exposing (Simulation, simulate, drawSim)
import Time exposing (fps)
import Signal exposing (foldp, map, (<~))
import Random exposing (initialSeed)
import Vec2 exposing (..)

sim : Simulation
sim = {
   quarry = {pos = (0, 0), v = (0,0), a = (0, 0)},
   target = (0,0),
   chaser = {pos = (0,0), v = (0, 0), a = (0, 0)},
   evader = {pos = (0,0), v = (0, 0), a = (0, 0)},
   rand = Random.pair (Random.float -1 1) (Random.float -1 1),
   seed = initialSeed 1337,
   reset = 10
  }

main = drawSim <~ foldp simulate sim (fps 60)
