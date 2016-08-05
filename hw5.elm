import PathFollowing exposing (..)
import Html.App exposing (program)
import AnimationFrame

main : Program Never
main = program
  { init = ( sim0, Cmd.none )
  , view = drawSim
  , update = update
  , subscriptions = always (AnimationFrame.diffs Tick)
  }
