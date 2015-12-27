import Vec2 exposing (..)
import ClassicalEngine exposing (OBR)
import Tanks exposing (stepTank, drawTank)
import Graphics.Collage exposing (collage)
import Maybe exposing (withDefault)

tank0 : OBR
tank0 =
 { o = (0, 0)
 , dir = (1, 0)
 , size = (20, 20)
 }

tanks : List OBR
tanks = List.foldl
  (\dt tt -> stepTank dt (16, -16) (withDefault tank0 (List.head tt)) :: tt)
  [tank0] (List.repeat 3 1)


main = collage 400 300 (List.foldl (\tank ff -> drawTank tank ++ ff) [] tanks)