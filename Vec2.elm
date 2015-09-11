module Vec2 where

type alias Vec2 = (Float, Float)
(.+.) : Vec2 -> Vec2 -> Vec2
(.+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
infixl 6 .+.

(.-.) : Vec2 -> Vec2 -> Vec2
(.-.) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
infixl 6 .-.

(.*) : Vec2 -> Float -> Vec2
(.*) (x, y) a = (x * a, y * a)
infixl 7 .*

(*.) : Float -> Vec2 -> Vec2
(*.) = flip (.*)
infixl 7 *.

(./) : Vec2 -> Float -> Vec2
(./) (x, y) a = (x / a, y / a)
infixl 7 ./

dot : Vec2 -> Vec2 -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

cross : Vec2 -> Vec2 -> Float
cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

norm : Vec2 -> Float
norm p = sqrt (dot p p)

dist : Vec2 -> Vec2 -> Float
dist p q = norm (p .-. q)

normalize : Vec2 -> Vec2
normalize p = p ./ norm p

project : Vec2 -> Vec2 -> Vec2
project p q = dot p q / dot q q *. q

reflect : Vec2 -> Vec2 -> Vec2
reflect p q = q .-. 2 *. project p q

min2 : Vec2 -> Vec2 -> Vec2
min2 (x1, y1) (x2, y2) = (min x1 x2, min y1 y2)

max2 : Vec2 -> Vec2 -> Vec2
max2 (x1, y1) (x2, y2) = (max x1 x2, max y1 y2)

clamp2 : Float -> Float -> Vec2 -> Vec2
clamp2 min max p = let n = norm p in
  if n < min then p .* min ./ n else
    if n > max then p .* max ./ n else p
