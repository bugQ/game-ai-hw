import Vec2 exposing (..)
import ClassicalEngine exposing (OBR)
import Tanks exposing (Tank, stepTank, drawTank)
import Graphics.Collage exposing (collage)

import Set exposing (Set)
import Char exposing (KeyCode)
import Keyboard
import Time exposing (Time, fps)

treadMax : Float
treadMax = 100

tankKeys : List (Char, Vec2)
tankKeys =
 [ ('Q', (treadMax, 0))
 , ('A', (-treadMax, 0))
 , ('W', (0, treadMax))
 , ('S', (0, -treadMax))
 ]

tankControl : (Set KeyCode) -> Vec2
tankControl keys = List.foldl
  (\(c, treads) sum ->
    if Set.member (Char.toCode c) keys then
      sum .+. treads
    else
      sum
  ) (0, 0) tankKeys

type Action = Tick Time | Levers Vec2

inputs : Signal Action
inputs = Signal.merge
  (Signal.map Tick (fps 60))
  (Signal.map Levers (Signal.map tankControl Keyboard.keysDown))

update : Action -> Tank -> Tank
update action tank = case action of
  Tick tick -> stepTank (Time.inSeconds tick) tank
  Levers levers -> { tank | treads = levers }

tank0 : Tank
tank0 =
 { o = (0, 0)
 , dir = (1, 0)
 , size = (20, 20)
 , treads = (0, 0)
 }

main = Signal.map
  (collage 400 300 << drawTank)
  (Signal.foldp update tank0 inputs)
