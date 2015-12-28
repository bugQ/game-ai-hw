import Time exposing (Time, fps)
import Signal exposing (message, (<~))
import Random exposing (initialSeed)
import Flocking exposing (Simulation, simulate, initSim, drawSim, defaults)
import Html exposing (..)
import Html.Attributes exposing (value, type', checked)
import Html.Events exposing (on, onClick, targetValue, targetChecked)
import Effects exposing (Effects)
import StartApp exposing (..)
import String
import Char

(<~) = Signal.map
(~) = Signal.map2 (<|)
infixl 4 <~
infixl 4 ~


type Action =
  Tick Time
  | Neighbor Float
  | Separate Float
  | Align Float
  | Cohere Float
  | DrawVectors Bool
  | Default
  | Reset
  | Nop

flockInput address default actionType = input [
  value (toString default),
  on "input" targetValue
    (\a -> Signal.message address (case (String.toFloat a) of
      Ok c -> actionType c
      Err s -> Nop))
 ] []

flockToggle address default actionType = input [
  type' "checkbox",
  checked default,
  on "change" targetChecked
    (\a -> Signal.message address (actionType a))
 ] []

code = Char.fromCode >> String.fromChar
oneThird = code 0x2153
twoThirds = code 0x2154

view address sim = div [] [
  drawSim sim |> fromElement,
  hr [] [],
  p [] [
    button [ onClick address Default ] [ text "Default" ],
    button [ onClick address Reset ] [ text "Reset" ]
   ],
  p [] [
    text "draw velocity vectors",
    flockToggle address sim.params.drawVectors DrawVectors
   ],
  dl [] [
    dt [] [text "neighborhood radius"],
     dd [] [flockInput address sim.params.neighborhoodRadius Neighbor],
    dt [] [text <| "separation (uses " ++ oneThird ++ " radius)"],
     dd [] [flockInput address sim.params.separation Separate],
    dt [] [text <| "alignment (uses full radius)"],
     dd [] [flockInput address sim.params.alignment Align],
    dt [] [text <| "coherence (uses " ++ twoThirds ++ " radius)"],
     dd [] [flockInput address sim.params.coherence Cohere]
   ]
 ]

init : Simulation
init = initSim (initialSeed 1337)

update action sim = let params = sim.params in
 (flip (,)) Effects.none <| case action of
  Tick dt -> simulate dt sim
  Reset -> { init | params <- sim.params }
  Default -> { sim | params <- defaults }
  Neighbor r -> { sim | params <- { params | neighborhoodRadius <- r } }
  Separate c -> { sim | params <- { params | separation <- c } }
  Align c -> { sim | params <- { params | alignment <- c } }
  Cohere c -> { sim | params <- { params | coherence <- c } }
  DrawVectors b -> { sim | params <- { params | drawVectors <- b } }

app = start {
  init = ( init, Effects.none ),
  update = update,
  view = view,
  inputs = [ Tick <~ (fps 60) ]
 }

main = app.html
