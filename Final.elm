import MonsterGame exposing (..)
import Vec2 exposing (Vec2, (.-.), (./))
import Collage exposing (collage)
import Element exposing (toHtml)
import Random exposing (generate)
import Time exposing (Time)
import Html exposing (Html)
import Html.App exposing (program)
import Mouse
import AnimationFrame

type Action = Init Game | Tick Time | Haunt Mouse.Position

canvasW : Int
canvasW = 600
canvasH : Int
canvasH = 600

view : Game -> Html Action
view = drawGame >> collage canvasW canvasH >> toHtml

mouseToCanvas : Mouse.Position -> Vec2
mouseToCanvas {x, y} = (toFloat (x - canvasW//2), toFloat (canvasH//2 - y))

update : Action -> Game -> (Game, Cmd Action)
update action game =
  ( case action of
      Init g -> g
      Tick dt -> run (Time.second / 60) game
      Haunt mouse -> controlMonster (mouseToCanvas mouse) game
  , if game.reset > 0 then Cmd.none else generate Init genGame )

main : Program Never
main = program
  { init = ( game0, generate Init genGame )
  , view = view
  , update = update
  , subscriptions = always <| Sub.batch
    [ AnimationFrame.diffs Tick
    , Mouse.clicks Haunt
    ]
  }
