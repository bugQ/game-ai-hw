module Shuffle where

import List exposing (map2, sortBy, unzip)
import Array exposing (Array, length, initialize, fromList, toList, get)
import Random exposing (Seed, generate)

shuffle : Seed -> Array a -> (Array a, Seed)
shuffle seed0 aa  = let
  gen = Random.list (length aa) (Random.float 0.0 1.0)
  (floats, seed1) = generate gen seed0
  randa = map2 (,) (toList aa) floats |> sortBy snd |> unzip |> fst |> fromList
 in
  (randa, seed1)
