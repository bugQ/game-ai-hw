module Tanks exposing (Tank, Simulation,
  sim0, genSim, genGen, stepTank, simulate, drawMine, drawSim)

import Vec2 exposing (..)
import Genetics exposing (..)
import Color exposing (red, green, grey)
import Collage exposing (Form, filled, outlined, rect, solid, rotate)
import ClassicalEngine exposing
  (Circle, OBR, wrap2, collideOBRxCircle, drawObstacle, drawOBR)
import Random exposing (Generator)
import Time exposing (Time)
import Set exposing (Set)

--- CONSTANTS ---

-- mines are circles with this radius
mineSize : Float
mineSize = 5

-- tanks are squares with this width
tankSize : Float
tankSize = 20

-- there are this many mines in the play area
numMines : Int
numMines = 40

-- this many players, including one human, each control a tank
numTanks : Int
numTanks = 16

-- each round lasts this long
genTime : Time
genTime = 30 * Time.second

-- the score is visible for this long after each round
overTime : Time
overTime = 3 * Time.second

-- bots vary their movement at this interval
moveTime : Time
moveTime = 0.5 * Time.second

-- the left and right tank treads may each go this fast
treadMax : Float
treadMax = 70

-- bots must remain within this radius to gain fitness points
fitnessRadius : Float
fitnessRadius = 100

-- a bot receives this many points for disarming a mine
fitnessBonus : Float
fitnessBonus = 100

-- the ratio of children which are not direct copies of their parents
crossoverChance : Float
crossoverChance = 0.6


numMoves : Int
numMoves = ceiling (genTime / moveTime)


--- STRUCTURES ---

type alias Tank = OBR
 { treads : Vec2
 , inv : Set Vec2
 , moves : List Vec2
 , history : List Vec2
 , fitness : Float
 , next : Time
 }

type alias Simulation =
 { size : Vec2
 , tanks : List Tank
 , mines : List Circle
 , reset : Time
 , generation : Int
 }


--- SIMULATION ---

tank0 : Tank
tank0 =
 { o = (0, 0)
 , dir = (1, 0)
 , size = (tankSize, tankSize)
 , treads = (0, 0)
 , inv = Set.empty
 , moves = []
 , history = []
 , fitness = 0
 , next = moveTime
 }

sim0 : Simulation
sim0 =
 { size = (0, 0)
 , tanks = []
 , mines = []
 , reset = Time.hour
 , generation = 0
 }

-- returns a random generator that can generate a fully initialized simulation
genSim : Float -> Float -> Generator Simulation
genSim w h = let
  genMines = Random.list numMines
    <| Random.map (\pos -> { o = pos, r = mineSize })
    <| Vec2.random (-w*0.5, -h*0.5) (w*0.5, h*0.5)
  genTanks = Random.list (numTanks - 1)
    <| Random.map (\moves -> { tank0 | moves = moves })
    <| Random.list numMoves
    <| Vec2.random (-1, -1) (1, 1)
 in
  Random.map2 (\mines tanks ->
      { size = (w, h)
      , tanks = tank0 :: tanks
      , mines = mines
      , reset = genTime
      , generation = 1
      }) genMines genTanks

-- advances the simulation by one frame, or restarts if time is up
simulate : Time -> Simulation -> Simulation
simulate tick sim = let sim = { sim | reset = sim.reset - tick } in
  if sim.reset > 0 then stepSim tick sim else sim

-- returns a generator for the next genetic generation in the simulation
genGen : Simulation -> Generator Simulation
genGen sim = let sim = restart sim in
  case sim.tanks of
    [] -> Random.map (always sim) Random.bool
    player :: bots -> case List.sortBy .fitness bots of
      [] -> Random.map (always sim) Random.bool
      elite :: rest -> let
        genCrossoverParams = Random.map3 (,,)
          (Random.float 0 1) (Random.float 0 1) (Random.float 0 1)
        genMutateParams = Random.list numMoves (Vec2.random (0, 0) (1, 1))
        genGenParams = Random.list (numTanks - 2)
          (Random.map2 (,) genCrossoverParams genMutateParams)
       in
        Random.map (\rand -> { sim
            | tanks = player :: elite :: List.map
              (\(pqr, uus) -> crossoverTanks sim.tanks pqr |> mutateTank uus)
              rand
            , generation = sim.generation + 1
            }) genGenParams

crossoverTanks : List Tank -> (Float, Float, Float) -> Tank
crossoverTanks tanks (p, q, r) = let
  parent1 = selectRanked tanks p |> Maybe.withDefault tank0
  parent2 = selectRanked tanks q |> Maybe.withDefault tank0
 in
  if r < crossoverChance
    then { parent1 | moves =
      crossover (r / crossoverChance) parent1.moves parent2.moves }
    else parent1

mutateTank : List Vec2 -> Tank -> Tank
mutateTank uus tank = { tank | moves =
  mutateVec2s ((-1, -1), (1, 1)) 21 uus tank.moves }


-- resets tanks and round timer
restart : Simulation -> Simulation
restart sim =
  { sim
  | reset = genTime
  , tanks = List.map (\tank ->
      { tank0 | moves = List.foldl (::) tank.moves tank.history }
    ) sim.tanks
  }

mutate : Tank -> Tank
mutate tank = tank

-- advances the simulation by one frame
stepSim : Time -> Simulation -> Simulation
stepSim tick sim = { sim | tanks = List.map ((stepTank tick)
      >> (\tank -> { tank | o =
          wrap2 (sim.size .* -0.5) (sim.size .* 0.5) tank.o })
      >> (\tank ->  { tank | fitness =
          tank.fitness + (fitnessRadius - List.foldl
              (\mine -> min (dist tank.o mine.o)) fitnessRadius sim.mines)
            / fitnessRadius * Time.inSeconds tick })
      >> (\tank -> List.foldl (\mine tank -> case collideOBRxCircle tank mine of
          Just _ -> { tank
            | inv = Set.insert mine.o tank.inv
            , fitness = tank.fitness +
                if Set.member mine.o tank.inv then 0 else fitnessBonus
            }
          Nothing -> tank
        ) tank sim.mines)
    ) sim.tanks
  }

--- BEHAVIOURS ---

-- moves tank treads, changes tread speeds based on move queue (unless empty)
stepTank : Time -> Tank -> Tank
stepTank tick tank = let
  nextMove = tank.next - tick
  tank = if nextMove > 0 then
      { tank | next = nextMove }
    else case tank.moves of
      move :: moves ->
        { tank
        | treads = move
        , moves = moves
        , history = move :: tank.history
        , next = nextMove + moveTime
        }
      [] -> { tank | next = 0 }
  dt = Time.inSeconds tick
  (treadL, treadR) = clamp2 -1 1 tank.treads .* treadMax
  width = fst tank.size
 in
  if treadL == treadR then
    if treadL == 0.0 then
      tank
    else
      { tank | o = perp tank.dir .* (treadR * dt) .+. tank.o }
  else if treadL == -treadR then
    { tank | dir = Vec2.rotate ((treadR - treadL) / width * dt) tank.dir }
  else let
    r = width / (1 - treadL / treadR) - width * 0.5
    pivot = tank.o .-. (r *. tank.dir)
    v = (treadL + treadR) * 0.5
    new_dir = Vec2.rotate (v / r * dt) tank.dir
    new_pos = pivot .+. (new_dir .* r)
   in
    { tank | o = new_pos, dir = new_dir }


--- DRAWING ---

-- draw a green square with a "barrel" rectangle pointing forward
drawTank : Tank -> List Form
drawTank tank = let
  angle = (atan2 (snd tank.dir) (fst tank.dir))
  barrel_offset = tank.size .*. (0.0, 0.5)
  barrel_pos = (tank.o .+. Vec2.rotate angle barrel_offset)
 in
  (filled green (uncurry rect (tank.size .*. (0.2, 0.5)))
    |> Collage.rotate angle
    |> Collage.move barrel_pos
  ) :: drawOBR (solid green) tank

-- draw a red circle
drawMine : Circle -> List Form
drawMine = drawObstacle red

-- draw all objects in the simulation
drawSim : Simulation -> List Form
drawSim sim = outlined (solid grey) (uncurry rect sim.size) ::
  List.foldl (drawTank >> (++)) [] sim.tanks ++
  List.foldl (drawMine >> (++)) [] sim.mines
