import Random exposing (initialSeed)
import Graphics.Collage exposing (collage)
import TreasureGame_BT exposing (Simulation, simulate, initSim, drawSim)
import Signal
import Time exposing (fps)

sim = initSim (initialSeed 1337)

main = Signal.map
  (collage 600 600 << drawSim)
  (Signal.foldp simulate sim (fps 50))
