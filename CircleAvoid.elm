module CircleAvoid where

import List exposing (map, foldr, filterMap, minimum)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Time exposing (Time, inSeconds)

import Vec2 exposing (..)
import OBR exposing (OBR, drawOBR)
import ChaseEvade exposing (Actor, drawActor, physics, maxV, maxA)

type alias Circle = {
  o : Vec2,
  r : Float
 }

-- whether they intersect, plus closest point on rect
collideOBRxCircle : OBR -> Circle -> Maybe Vec2
collideOBRxCircle obr circ = let p = OBR.nearestPoint obr circ.o in
  if sqnorm (p .-. circ.o) > circ.r then Nothing else Just p

terrain : List Circle
terrain = [
  {o = (-200, -200), r = 50},
  {o = (200, -200), r = 50},
  {o = (-200, 200), r = 50},
  {o = (200, 200), r = 50},
  {o = (50, -70), r = 80},
  {o = (-120, 120), r = 25},
  {o = (-95, 25), r = 40},
  {o = (12, 107), r = 50},
  {o = (123, 55), r = 30},
  {o = (-125, -75), r = 30},
  {o = (-75, -145), r = 13},
  {o = (123, 130), r = 13}
 ]

drawObstacle : Color -> Circle -> List Form
drawObstacle color circ = [
  move circ.o <| filled color <| circle circ.r,
  move circ.o <| outlined (solid charcoal) <| circle circ.r
 ]

futureProj : Actor -> OBR
futureProj actor = let
  sqv = sqnorm actor.v
  nx = actor.v ./ sqrt sqv
  rf = sqv / (4 * maxA)
  r = rf *. nx
 in
  { center = actor.pos .+. r, direction = nx, apothems = (rf, 10) }

type alias Driver = { vehicle : Actor, vision : OBR }
type alias Simulation = { driver : Driver, terrain : List Circle }

-- distance from actor and projected point of collision, or nothing
nearestFutureCollision : Simulation -> Maybe Vec2
nearestFutureCollision sim =
  filterMap (collideOBRxCircle sim.driver.vision) sim.terrain |>
    map (\p -> (norm (sim.driver.vehicle.pos .-. p), p)) |>
    minimum |> Maybe.map snd

avoid : Vec2 -> Driver -> Driver
avoid p driver = let
  r = (p .-. driver.vehicle.pos)
  dir = driver.vision.direction
  urgency = 2 * (fst driver.vision.apothems) - norm r
  steering = perp <| dir .*
    if (dir `cross` r) > 0 then urgency else -urgency
  braking = urgency * -urgency *. dir
  newA = steering .+. braking |> clamp2 0 maxA
  oldActor = driver.vehicle -- needed to get around parser limitation
  newActor = { oldActor | a <- newA } -- can't just put driver.actor here :\
 in
  { driver | vehicle <- newActor }

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
    Nothing -> sim.driver
  movedActor = physics dt evasiveDriver.vehicle
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

drawSim : Simulation -> Element
drawSim sim = collage 400 400 <|
  (foldr (++) (drawActor green sim.driver.vehicle) <|
   map (drawObstacle grey) sim.terrain) ++
  drawOBR (solid yellow) sim.driver.vision