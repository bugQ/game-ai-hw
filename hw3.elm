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

type Action =
  Tick Time
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
    (\a -> Signal.message address (DrawVectors a))
 ] []

view address sim = div [] [
  drawSim sim |> fromElement,
  hr [] [],
  p [] [
    button [ onClick address Reset ] [ text "Reset" ]
   ],
  p [] [
    text "draw velocity vectors",
    flockToggle address sim.params.drawVectors DrawVectors
   ],
  dl [] [
    dt [] [text "separation"],
     dd [] [flockInput address sim.params.separation Separate],
    dt [] [text "alignment"],
     dd [] [flockInput address sim.params.alignment Align],
    dt [] [text "coherence"],
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
