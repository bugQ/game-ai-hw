import PathFollowing exposing (..)
import Html.App exposing (program)
import Random exposing (generate)
import AnimationFrame

main : Program Never
main = program
  { init = ( sim0, generate Init initSim )
  , view = drawSim
  , update = update
  , subscriptions = always (AnimationFrame.diffs Tick)
  }
