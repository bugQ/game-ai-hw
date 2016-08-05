import Time exposing (Time)
import Random exposing (generate)
import Flocking exposing (..)
import Html exposing (..)
import Html.Attributes exposing (value, type', checked)
import Html.Events exposing (onInput, onCheck, onClick)
import Html.App exposing (program)
import Element exposing (toHtml)
import String
import Char
import AnimationFrame


type Action =
  Init Simulation
  | Tick Time
  | Neighbor Float
  | Separate Float
  | Align Float
  | Cohere Float
  | DrawVectors Bool
  | Default
  | Reset
  | Nop

init : Simulation -> Cmd Action
init sim = generate Init (initSim sim.params)

flockInput : Float -> (Float -> Action) -> Html Action
flockInput default actionType = input [
  value (toString default),
  onInput
    (\a -> case (String.toFloat a) of
      Ok c -> actionType c
      Err s -> Nop)
 ] []

flockToggle : Bool -> (Bool -> Action) -> Html Action
flockToggle default actionType = input [
  type' "checkbox",
  checked default,
  onCheck actionType
 ] []

code : Int -> String
code = Char.fromCode >> String.fromChar
oneThird : String
oneThird = code 0x2153
twoThirds : String
twoThirds = code 0x2154

view : Simulation -> Html Action
view sim = div [] [
  drawSim sim |> toHtml,
  hr [] [],
  p [] [
    button [ onClick Default ] [ text "Default" ],
    button [ onClick Reset ] [ text "Reset" ]
   ],
  p [] [
    text "draw velocity vectors",
    flockToggle sim.params.drawVectors DrawVectors
   ],
  dl [] [
    dt [] [text "neighborhood radius"],
     dd [] [flockInput sim.params.neighborhoodRadius Neighbor],
    dt [] [text <| "separation (uses " ++ oneThird ++ " radius)"],
     dd [] [flockInput sim.params.separation Separate],
    dt [] [text <| "alignment (uses full radius)"],
     dd [] [flockInput sim.params.alignment Align],
    dt [] [text <| "coherence (uses " ++ twoThirds ++ " radius)"],
     dd [] [flockInput sim.params.coherence Cohere]
   ]
 ]

update : Action -> Simulation -> (Simulation, Cmd Action)
update action sim = let params = sim.params in
 case action of
  Init sim -> (sim, Cmd.none)
  Tick dt -> (simulate dt sim, Cmd.none)
  Reset -> (sim, init sim)
  Default -> ({ sim | params = defaults }, Cmd.none)
  Neighbor r -> ({ sim | params = { params | neighborhoodRadius = r } }, Cmd.none)
  Separate c -> ({ sim | params = { params | separation = c } }, Cmd.none)
  Align c -> ({ sim | params = { params | alignment = c } }, Cmd.none)
  Cohere c -> ({ sim | params = { params | coherence = c } }, Cmd.none)
  DrawVectors b -> ({ sim | params = { params | drawVectors = b } }, Cmd.none)
  Nop -> (sim, Cmd.none)

main : Program Never
main = program {
  init = ( sim0, init sim0 ),
  update = update,
  view = view,
  subscriptions = always (AnimationFrame.diffs Tick)
 }
