import Tanks exposing (Simulation, Tank, simulate, drawSim)
import Collage exposing (collage)
import Element exposing (toHtml)

import Set exposing (Set)
import Char exposing (KeyCode)
import Keyboard
import Time exposing (Time)
import AnimationFrame
import Random exposing (initialSeed, generate)

import Html exposing (Html, div, p, text)
import Html.App exposing (program)

type Action = Init Simulation | Tick Time | LeverL Float | LeverR Float | Pass

keyDown : KeyCode -> Action
keyDown key = case key of
  81 -> LeverL 1 -- Q: left tread forward
  65 -> LeverL -1 -- A: left tread backward
  87 -> LeverR 1 -- W: right tread forward
  83 -> LeverR -1 -- S: right tread backward
  _ -> Pass

keyUp : KeyCode -> Action
keyUp key = case key of
  81 -> LeverL 0
  65 -> LeverL 0
  87 -> LeverR 0
  83 -> LeverR 0
  _ -> Pass

init : Cmd Action
init = generate Init (Tanks.genSim 400 300)

update : Action -> Simulation -> Simulation
update action sim = case action of
  Init newSim -> newSim
  Tick tick -> simulate tick sim
  LeverL l -> case sim.tanks of
    player :: bots ->
      { sim | tanks = { player | treads = (l, snd player.treads) } :: bots }
    [] -> sim
  LeverR r -> case sim.tanks of
    player :: bots ->
      { sim | tanks = { player | treads = (fst player.treads, r) } :: bots }
    [] -> sim
  Pass -> sim

view : Simulation -> Html Action
view sim = div []
  [ drawSim sim |> collage 400 300 |> toHtml
  , p [] [text (case sim.tanks of
      player :: bots -> toString (Set.size player.inv)
      [] -> ""
    )]
  ]

main : Program Never
main = program
  { init = ( Tanks.sim0, init )
  , update = (\a m -> (update a m, Cmd.none))
  , view = view
  , subscriptions = always <| Sub.batch
    [ AnimationFrame.diffs Tick
    , Keyboard.downs keyDown
    , Keyboard.ups keyUp
    ]
  }
