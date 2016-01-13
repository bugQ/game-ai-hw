import Vec2 exposing (..)
import ClassicalEngine exposing (OBR)
import Tanks exposing (Simulation, Tank, initSim, simulate, drawSim)
import Graphics.Collage exposing (collage)

import Set exposing (Set)
import Char exposing (KeyCode)
import Keyboard
import Time exposing (Time, fps)
import Random exposing (initialSeed)

import Html exposing (Html, div, p, text)
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


update : Action -> Simulation -> Simulation
update action sim = case action of
  Tick tick -> simulate tick sim
  Levers levers -> case sim.tanks of
    player :: bots ->
      { sim | tanks = { player | treads = levers } :: bots }
    [] -> sim

view : Signal.Address Action -> Simulation -> Html
view address sim = div []
  [ drawSim sim |> collage 400 300 |> Html.fromElement
  , p [] [text (case sim.tanks of
      player :: bots -> toString (Set.size player.inv)
      [] -> ""
    )]
  ]


main = .html <| start
  { init = ( initSim 400 300 (initialSeed 1337), Effects.none )
  , update = (\a m -> (update a m, Effects.none))
  , view = view
  , inputs =
    [ Tick <~ fps 60
    , Levers << tankControl <~ Keyboard.keysDown
    ]
  }
