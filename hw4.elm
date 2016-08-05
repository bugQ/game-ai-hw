import PathFinding exposing (Simulation, sim0, simulate, initSim, drawSim)
import Collage exposing (collage)
import Element exposing (toHtml)
import Time exposing (Time)
import Html exposing (Html)
import Html.App exposing (program)
import AnimationFrame

type Action = Init Simulation | Tick Time

view : Simulation -> Html Action
view = drawSim >> collage 600 600 >> toHtml

update : Action -> Simulation -> (Simulation, Cmd Action)
update action sim =
  ( case action of
      Init sim -> sim
      Tick dt -> simulate dt sim
  , Cmd.none
  )

main : Program Never
main = program
  { init = ( sim0, Cmd.none )
  , view = view
  , update = update
  , subscriptions = always (AnimationFrame.diffs Tick)
  }
