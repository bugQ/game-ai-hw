module PathFinding where

import Grid exposing (Grid, gridW, gridH, GridNode, drawGrid)
import Random exposing (Generator, generate, Seed)

maxBlocks = 60

type alias Simulation = {
  grid : Grid,
  rand : Generator (Int, Int),
  seed : Seed
}

initSim : Seed -> Simulation
initSim seed0 = let
  emptyGrid = Grid.repeat gridW gridH Grid.Traversable
  rand = (Random.pair (Random.int 0 gridW) (Random.int 0 gridH))
  (indices, seed1) = generate (Random.list maxBlocks rand) seed0
 in {
    grid = List.foldr Grid.set emptyGrid indices,
    rand = rand,
    seed = seed1
  }
