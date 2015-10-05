import Random exposing (initialSeed)
import Graphics.Collage exposing (collage)
import Grid exposing (drawGrid)
import PathFinding exposing (initSim)

sim = initSim (initialSeed 1337)

main = collage 500 500 (drawGrid sim.grid)
