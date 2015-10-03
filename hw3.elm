import Time exposing (Time, fps)
import Signal exposing (message, (<~))
import Random exposing (initialSeed)
import Flocking exposing (Simulation, simulate, initSim, drawSim, defaults)
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (on, targetValue)
import Effects exposing (Effects)
import StartApp exposing (..)
import String

type Action =
  Tick Time
  | Separate Float
  | Align Float
  | Cohere Float
  | Nop

flockInput address default actionType = input [
  on "input" targetValue
    (\a -> Signal.message address (case (String.toFloat a) of
      Ok c -> actionType c
      Err s -> Nop)),
  value (toString default) ] []

view address sim = div [] [
  drawSim sim |> fromElement,
  hr [] [],
  dl [] [
    dt [] [text "separation"],
     dd [] [flockInput address defaults.separation Separate],
    dt [] [text "alignment"],
     dd [] [flockInput address defaults.alignment Align],
    dt [] [text "coherence"],
     dd [] [flockInput address defaults.coherence Cohere]
   ]
 ]

update action sim = let params = sim.params in
 (flip (,)) Effects.none <| case action of
  Tick dt -> simulate dt sim
  Separate c -> { sim | params <- { params | separation <- c } }
  Align c -> { sim | params <- { params | alignment <- c } }
  Cohere c -> { sim | params <- { params | coherence <- c } }

sim : Simulation
sim = initSim (initialSeed 1337)

none : Effects Action
none = Effects.none

app = start {
  init = ( sim, none ),
  update = update,
  view = view,
  inputs = [ Tick <~ (fps 60) ]
 }

main = app.html
