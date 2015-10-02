module CircleAvoid where

import List exposing (map, foldl, filterMap, minimum)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Time exposing (Time, inSeconds)

import Vec2 exposing (..)
import ClassicalEngine exposing (..)


--- Structures ---

type alias Driver = {
  vehicle : Actor,
  vision : OBR  -- a box represents their line of sight
}

type alias Simulation = {
  driver : Driver,
  terrain : List Circle
}


--- Constants ---

terrain : List Circle
terrain = [
  {o = (-200, -200), r = 50},
  {o = (200, -200), r = 50},
  {o = (-200, 200), r = 50},
  {o = (200, 200), r = 50},
  {o = (50, -70), r = 80},
  {o = (-120, 120), r = 25},
  {o = (-85, 25), r = 40},
  {o = (12, 107), r = 50},
  {o = (123, 55), r = 30},
  {o = (-125, -75), r = 30},
  {o = (-75, -145), r = 13},
  {o = (123, 130), r = 13}
 ]

maxA = 80
maxV = 120

maxBrakeTime = maxV / (2 * maxA)


-- Behavior --

-- generates line of sight based on current speed
futureProj : Actor -> OBR
futureProj actor = let
  r = maxBrakeTime *. actor.v
  d = norm r
  n = r ./ d
 in
  { o = actor.pos .+. r ./ 2, dir = n, size = (d, 16) }

-- distance from actor and projected point of collision, or nothing
nearestFutureCollision : Simulation -> Maybe Vec2
nearestFutureCollision sim =
  filterMap (collideOBRxCircle sim.driver.vision) sim.terrain |>
    map (\p -> (norm (sim.driver.vehicle.pos .-. p), p)) |>
    minimum |> Maybe.map snd

avoid : Vec2 -> Driver -> Driver
avoid p driver = let
  r = p .-. driver.vehicle.pos
  dir = driver.vision.dir
  urgency = fst driver.vision.size / norm r / 2
  avoidV = perp dir .* if dir `cross` r > 0 then -urgency else urgency
  steering = avoidV .* maxV .-. driver.vehicle.v
  braking = urgency * -urgency *. dir
  newA = 2 * maxA *. (steering .+. braking) |> clamp2 0 maxA
  oldActor = driver.vehicle -- needed to get around parser limitation
  newActor = { oldActor | a <- newA } -- can't just put driver.actor here :\
 in
  { driver | vehicle <- newActor }

accel : Driver -> Driver
accel driver = let oldActor = driver.vehicle in
  { driver | vehicle <- { oldActor | a <- normalize oldActor.v .* maxA } }


-- Simulation --

-- toroidal wrapping (modulus-like)
wrap : Float -> Float -> Float -> Float
wrap min max x = let len = max - min in
  if x < min then x + len else if x > max then x - len else x
wrap2 : Vec2 -> Vec2 -> Vec2 -> Vec2
wrap2 min max p = (
  wrap (fst min) (fst max) (fst p), wrap (snd min) (snd max) (snd p))

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = (inSeconds t)
  evasiveDriver = case (nearestFutureCollision sim) of
    Just p -> sim.driver |> avoid p
    Nothing -> sim.driver |> accel
  movedActor = stepActor maxV dt evasiveDriver.vehicle
  newActor = { movedActor |
    pos <- wrap2 (-200, -200) (200, 200) movedActor.pos }
  newOBR = futureProj newActor
 in { sim | driver <-
   { evasiveDriver | vehicle <- newActor, vision <- newOBR } }

initSim : Simulation
initSim = let initActor = { pos = (0, 10), v = (maxV, 0), a = (0, 0) } in
 {
  driver = {
    vehicle = initActor,
    vision = futureProj initActor
   },
  terrain = terrain
 }


-- Drawing --

drawSim : Simulation -> Element
drawSim sim = collage 400 400 <|
  (foldl (++) (drawVehicle green sim.driver.vehicle) <|
   map (drawObstacle grey) sim.terrain) ++
  drawOBR (solid yellow) sim.driver.vision
