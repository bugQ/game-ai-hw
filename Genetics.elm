module Genetics exposing (..)

import Vec2 exposing (Vec2)
import List exposing (length, head, take, drop, repeat, indexedMap)

type alias Chromosome a = List a

-- performs one-point crossover on two lists
-- u is presumed uniform random parameter from [0, 1)
crossover : Float -> Chromosome a -> Chromosome a -> Chromosome a
crossover u parent1 parent2 = let cut l = toFloat (length l) * u |> round in
  take (cut parent1) parent1 ++ drop (cut parent2) parent2

-- generates the points used in stochastic universal sampling
-- randomly chooses n evenly spaced points that are 1/n apart
-- u is presumed uniform random parameter from [0, 1)
sus : Int -> Float -> List Float
sus n u = let r x = x / (toFloat n) in
  indexedMap (toFloat >> r >> (+)) (repeat n (r u))

-- performs ranked selection with the Benford's Law distribution
-- u is presumed uniform random parameter from 0 to 1
selectRanked : List b -> Float -> Maybe b
selectRanked parents u = let n = toFloat (List.length parents) in
  head (drop (floor (n^u)) parents)

-- performs ranked selection with Benford's Law distribution
-- then performs one-point crossover
crossoverRanked : Float -> List (Chromosome a) -> (Float, Float, Float) -> Chromosome a
crossoverRanked crossoverChance parents (p, q, r) = let
  parent1 = selectRanked parents p |> Maybe.withDefault []
  parent2 = selectRanked parents q |> Maybe.withDefault []
 in
  if r < crossoverChance
    then crossover (r / crossoverChance) parent1 parent2
    else parent1

-- performs polynomial mutation on a single real-valued gene (given exponent)
-- keeps x within [xmin, xmax]
-- u presumed uniform random parameter from [0, 1)
mutateReal : (Float, Float) -> Float -> Float -> Float -> Float
mutateReal (xmin, xmax) exp u x = let
  base = 2 * min u (1 - u)
  span = x - if u > 0.5 then xmax else xmin
 in
  (base^exp - 1) * span + x

-- mutates a real-valued chromosome (list of real-valued genes)
-- with mutation chance of 1/n per gene for n genes (average 1 mutation)
-- us presumed list of n uniform random parameters from [0, 1)
mutateReals : (Float, Float) -> Float -> List Float -> Chromosome Float -> Chromosome Float
mutateReals (xmin, xmax) exp us xs = let
  n = toFloat (List.length xs)
  mutationChance = 1 / n
  mutate = mutateReal (xmin, xmax) exp
 in
  List.map2 (\u gene ->
      if u < mutationChance then mutate (u * n) gene else gene
    ) us xs

-- performs polynomial mutation on a real-valued pair (given exponent)
-- keeps x within [xmin, xmax] and y within [ymin, ymax]
-- ux and uy presumed uniform random parameters from [0, 1)
mutateVec2 : (Vec2, Vec2) -> Float -> Vec2 -> Vec2 -> Vec2
mutateVec2 ((xmin, ymin), (xmax, ymax)) exp (ux, uy) (x, y) =
  (mutateReal (xmin, xmax) exp ux x, mutateReal (ymin, ymax) exp uy y)

-- mutates a 2D real-valued chromosome (list of 2-vector genes)
-- with mutation chance of 1/n per gene for n genes (average 1 mutation)
-- uus presumed list of n pairs of uniform random parameters from [0, 1)
mutateVec2s : (Vec2, Vec2) -> Float -> List Vec2 -> Chromosome Vec2 -> Chromosome Vec2
mutateVec2s (vmin, vmax) exp uus vs = let
  n = toFloat (List.length vs)
  mutationChance = 1 / n
  mutate = mutateVec2 (vmin, vmax) exp
 in
  List.map2 (\(ux, uy) gene ->
      if ux < mutationChance then mutate (ux * n, uy) gene else gene
    ) uus vs
