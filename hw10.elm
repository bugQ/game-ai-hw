import Vec2 exposing (..)
import ClassicalEngine exposing (OBR)
import Tanks exposing (Tank, stepTank, drawTank)
import Graphics.Collage exposing (collage)

import Set exposing (Set)
import Char exposing (KeyCode)
import Keyboard
import Time exposing (Time, fps)

import Html exposing (Html, div)
import Effects
import StartApp exposing (start)

(<~) = Signal.map
(~) = Signal.map2 (<|)
infixl 4 <~
infixl 4 ~


type Action = Tick Time | Levers Vec2


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


update : Action -> Tank -> Tank
update action tank = case action of
  Tick tick -> stepTank (Time.inSeconds tick) tank
  Levers levers -> { tank | treads = levers }

view : Signal.Address Action -> Tank -> Html
view address = drawTank >> collage 400 300 >> Html.fromElement

tank0 : Tank
tank0 =
 { o = (0, 0)
 , dir = (1, 0)
 , size = (20, 20)
 , treads = (0, 0)
 }


main = .html <| start
 { init = ( tank0, Effects.none )
 , update = (\a m -> (update a m, Effects.none))
 , view = view
 , inputs =
   [ Tick <~ fps 60
   , Levers << tankControl <~ Keyboard.keysDown
   ]
 }
