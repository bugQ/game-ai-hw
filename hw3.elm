import Time exposing (fps)
import Signal exposing (foldp, map, (<~))
import Random exposing (initialSeed)
import Flocking exposing (Simulation, simulate, initSim, drawSim)

main = drawSim <~ foldp simulate (initSim (initialSeed 1337)) (fps 60)