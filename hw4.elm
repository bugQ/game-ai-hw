import Random exposing (initialSeed)
import Graphics.Collage exposing (collage)
import PathFinding exposing (initSim, simulate, drawSim)
import Signal exposing (foldp, (<~))
import Time exposing (fps)

sim = initSim (initialSeed 1337)

main = collage 500 500 << drawSim <~ foldp simulate sim (fps 10)
