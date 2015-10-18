import Random exposing (initialSeed)
import Graphics.Collage exposing (collage)
import PathFollowing exposing (Simulation, simulate, initSim, drawSim)
import Signal exposing (foldp, (<~))
import Time exposing (fps)
import Html

sim = initSim (initialSeed 1337)

main = collage 600 600 (drawSim sim)