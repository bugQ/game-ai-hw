import NeuroTanks as Tanks exposing (Simulation, Tank, simulate, drawSim)
import Collage exposing (collage)
import Element exposing (toHtml)

import Set exposing (Set)
import Char exposing (KeyCode)
import String
import Keyboard
import Time exposing (Time, inSeconds)
import AnimationFrame
import Random exposing (initialSeed, generate)

import Html exposing (Html, div, p, h2, h3, ol, li, text)
import Html.Attributes exposing (style)
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

regen : Simulation -> Cmd Action
regen sim = generate Init (Tanks.genGen sim)

update : Action -> Simulation -> (Simulation, Cmd Action)
update action sim = case action of
  Init newSim -> (newSim, Cmd.none)
  Tick tick -> (simulate tick sim
    , if sim.reset < -3 * Time.second then regen sim else Cmd.none)
  LeverL l -> (case sim.tanks of
      player :: bots ->
        { sim | tanks = { player | treads = (l, snd player.treads) } :: bots }
      [] -> sim
    , Cmd.none)
  LeverR r -> (case sim.tanks of
      player :: bots ->
        { sim | tanks = { player | treads = (fst player.treads, r) } :: bots }
      [] -> sim
    , Cmd.none)
  Pass -> (sim, Cmd.none)

view : Simulation -> Html Action
view sim = let
  hiscore = List.foldl (.fitness >> max) 0 sim.tanks
 in
  Html.main' [style [("width", "600px")]] (
    [ div [style [("float", "right"), ("width", "180px")]]
      [ h3 [] [text "Time"]
      , p [] [text <| String.left 4 <| toString <| inSeconds sim.reset]
      , h3 [] [text "Scores"]
      , ol [] (List.map (\tank ->
            li [style (if tank.fitness == hiscore
                  then [("font-weight", "bold")] else [])]
              [text (toString (Set.size tank.inv) ++
                " (fitness " ++ String.left 7 (toString tank.fitness) ++ ")")]
          ) sim.tanks)
      ]
    , drawSim sim |> collage 400 300 |> toHtml
    , h3 [] [text ("Generation " ++ toString sim.generation)]
    ] ++ if sim.reset > 0 then [] else [h2 [] [text "Round over"]])

main : Program Never
main = program
  { init = ( Tanks.sim0, init )
  , update = update
  , view = view
  , subscriptions = always <| Sub.batch
    [ AnimationFrame.diffs Tick
    , Keyboard.downs keyDown
    , Keyboard.ups keyUp
    ]
  }
