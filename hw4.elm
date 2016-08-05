import PathFinding exposing (Simulation, sim0, simulate, initSim, drawSim)
import Collage exposing (collage)
import Element exposing (toHtml)
import Time exposing (Time, every, second)
import Html exposing (Html)
import Html.App exposing (program)
import Random exposing (Generator, generate)

type Action = Init Simulation | Tick Time

view : Simulation -> Html Action
view = drawSim >> collage 600 600 >> toHtml

update : Action -> Simulation -> (Simulation, Cmd Action)
update action sim =
  ( case action of
      Init sim -> sim
      Tick dt -> simulate sim
  , if sim.restart > 0 then Cmd.none else generate Init initSim
  )

main : Program Never
main = program
  { init = ( sim0, Cmd.none )
  , view = view
  , update = update
  , subscriptions = always (every (second / 10) Tick)
  }
