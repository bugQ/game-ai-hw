module CircleAvoid where

import List exposing (map, foldr, filterMap, minimum)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Time exposing (Time, inSeconds)

import Vec2 exposing (..)
import OBR exposing (OBR, drawOBR)
import ChaseEvade exposing (Actor, drawActor, physics, maxV, maxA)

type alias Circle = {
  o : Vec2,
  r : Float
 }

-- whether they intersect, plus closest point on rect
colldeOBRxCircle : OBR -> Circle -> Maybe Vec2
colldeOBRxCircle obr circ = let p = OBR.nearestPoint obr circ.o in
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
  filterMap (colldeOBRxCircle sim.driver.vision) sim.terrain |>
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

simulate : Time -> Simulation -> Simulation
simulate t sim = let
  dt = (inSeconds t)
  evasiveDriver = case (nearestFutureCollision sim) of
    Just p -> sim.driver |> avoid p
    Nothing -> sim.driver
 in { sim | driver <-
   { evasiveDriver | vehicle <- physics dt evasiveDriver.vehicle } }
