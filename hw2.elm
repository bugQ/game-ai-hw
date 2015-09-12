import Time exposing (fps)
import Signal exposing (foldp, map, (<~))
import Random exposing (initialSeed)
import Vec2 exposing (..)

import CircleAvoid exposing (Simulation, simulate, initSim, drawSim)

main = drawSim <~ foldp simulate initSim (fps 60)