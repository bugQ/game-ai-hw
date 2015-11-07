import Random exposing (initialSeed)
import Graphics.Collage exposing (collage)
import TreasureGame exposing (Simulation, simulate, initSim, drawSim)
import Signal exposing (foldp, (<~))
import Time exposing (fps)
import Html

sim = initSim (initialSeed 1337)

main = collage 600 600 << drawSim <~ foldp simulate sim (fps 50)
