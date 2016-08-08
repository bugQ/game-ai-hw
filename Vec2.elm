module Vec2 exposing (..)

import Random exposing (Generator)

-- floating-point only, for simplicity
type alias Vec2 = (Float, Float)

-- vector addition
(.+.) : Vec2 -> Vec2 -> Vec2
(.+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
infixl 6 .+.

-- vector subtraction
(.-.) : Vec2 -> Vec2 -> Vec2
(.-.) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
infixl 6 .-.

-- non-uniform vector scaling (component multiplication)
(.*.) : Vec2 -> Vec2 -> Vec2
(.*.) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
infixl 7 .*.

-- uniform vector scaling (scalar multiplication)
(.*) : Vec2 -> Float -> Vec2
(.*) (x, y) a = (x * a, y * a)
infixl 7 .*

-- uniform vector scaling (scalar multiplication)
(*.) : Float -> Vec2 -> Vec2
(*.) = flip (.*)
infixl 7 *.

-- uniform vector scaling (scalar division)
(./) : Vec2 -> Float -> Vec2
(./) (x, y) a = (x / a, y / a)
infixl 7 ./

-- dot product (scalar product)
dot : Vec2 -> Vec2 -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- since 2D, gives magnitude (z) of cross product
cross : Vec2 -> Vec2 -> Float
cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

-- opposite vector
oppose : Vec2 -> Vec2
oppose (x, y) = (-x, -y)

-- right-handed (CCW) perpendicular
perp : Vec2 -> Vec2
perp (x, y) = (-y, x)

-- angle of vector (radians) relative to x axis
angle : Vec2 -> Float
angle (x, y) = atan2 y x

-- norm = magnitude = length
norm : Vec2 -> Float
norm p = sqrt (dot p p)

-- square of norm.  easier to calculate
sqnorm : Vec2 -> Float
sqnorm p = dot p p

-- distance between points, length of vector difference
dist : Vec2 -> Vec2 -> Float
dist p q = norm (p .-. q)

-- unit vector in direction of given vector
normalize : Vec2 -> Vec2
normalize p = p ./ norm p

-- vector projection (component of p in direction of q)
project : Vec2 -> Vec2 -> Vec2
project p q = dot p q / dot q q *. q

-- reflection of point/vector p around direction of q
reflect : Vec2 -> Vec2 -> Vec2
reflect p q = q .-. 2 *. project p q

-- rotate vector around origin by angle
rotate : Float -> Vec2 -> Vec2
rotate rad p = let b = (sin rad, cos rad) in (cross p b, dot p b)

-- minimum of each vector component
min2 : Vec2 -> Vec2 -> Vec2
min2 (x1, y1) (x2, y2) = (min x1 x2, min y1 y2)

-- maximum of each vector component
max2 : Vec2 -> Vec2 -> Vec2
max2 (x1, y1) (x2, y2) = (max x1 x2, max y1 y2)

-- vector p with magnitude constrained by max and min
clamp2 : Float -> Float -> Vec2 -> Vec2
clamp2 min max p = let n = norm p in
  if n > max then p .* max ./ n else
    if n < min then p .* min ./ n else p

-- mean (average) of vectors
mean2 : List Vec2 -> Vec2
mean2 ps = List.foldl (.+.) (0, 0) ps ./ toFloat (List.length ps)


random : Vec2 -> Vec2 -> Generator Vec2
random (xmin, ymin) (xmax, ymax) =
  Random.map2 (,) (Random.float xmin xmax) (Random.float ymin ymax)
