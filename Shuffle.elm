module Shuffle where

import List exposing (indexedMap, sortBy, unzip)
import Array exposing (Array, length, initialize, fromList, get)
import Random exposing (Seed, generate)

shuffle : Seed -> Array a -> (Array a, Seed)
shuffle seed0 aa  = let
  get i xx = case Array.get i xx of
    Just x -> x
  n = length aa
  gen = Random.list n (Random.float 0.0 1.0)
  (floats, seed1) = generate gen seed0
  randices = indexedMap (,) floats |> sortBy snd |> unzip |> fst |> fromList
 in
  (Array.initialize n (\i -> get (get i randices) aa), seed1)
