import Grid exposing (Grid)
import Random exposing (Generator, generate, initialSeed, Seed)

gridW = 15
gridH = 15
maxBlocks = 40

type alias Simulation = {
  grid : Grid,
  rand : Generator (Int, Int),
  seed : Seed
}

initSim : Seed -> Simulation
initSim seed0 = let
  emptyGrid = Grid.repeat gridW gridH Traversable
  rand = (Random.pair (Random.int 0 gridW) (Random.int 0 gridH))
  (indices, seed1) = generate (Random.list maxBlocks rand) seed0
 in {
    grid = foldr Grid.set emptyGrid indices,
    rand = rand,
    seed = seed1
  }

sim = initSim (initialSeed 1337)

main = collage 500 500 (drawGrid sim.grid)
