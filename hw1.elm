import ChaseEvade exposing (..)
import Element exposing (toHtml)
import Html exposing (Html)
import Html.App exposing (program)
import AnimationFrame

view : Simulation -> Html Action
view = drawSim >> toHtml

update : Action -> Simulation -> (Simulation, Cmd Action)
update action sim = case action of
  Init sim -> (sim, Cmd.none)
  Tick dt -> simulate dt sim

main : Program Never
main = program
  { init = ( sim0, Cmd.none )
  , view = view
  , update = update
  , subscriptions = always (AnimationFrame.diffs Tick)
  }
