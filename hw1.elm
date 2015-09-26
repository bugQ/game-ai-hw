import Time exposing (fps)
import Signal exposing (foldp, map, (<~))
import Random exposing (initialSeed)
import Vec2 exposing (..)
import ChaseEvade exposing (Simulation, simulate, initSim, drawSim)

rand = Random.pair (Random.float -1 1) (Random.float -1 1)
seed = initialSeed 1337

main = drawSim <~ foldp simulate (initSim rand seed) (fps 60)
