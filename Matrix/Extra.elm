module Matrix.Extra exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Matrix exposing (..)

transpose : Matrix a -> Matrix a
transpose a = case Array.get 0 a of
  Nothing -> Array.empty
  Just row0 -> Array.indexedMap (\i a0i ->
      Array.map (\row -> case Array.get i row of
          Nothing -> a0i
          Just aji -> aji
        ) a
    ) row0

dotVector : Array number -> Array number -> number
dotVector us vs = Array.foldl (+) 0 <| Array.indexedMap
  (\i u -> case Array.get i vs of
    Just v -> u * v
    Nothing -> 0) us

dotColumn : Matrix number -> Array number -> Array number
dotColumn a x = Array.map (dotVector x) a

map2 : (a -> b -> result) -> Matrix a -> Matrix b -> Matrix result
map2 = Array.map2 << Array.map2

map3
  : (a -> b -> c -> result)
  -> Matrix a -> Matrix b -> Matrix c -> Matrix result
map3 = Array.map3 << Array.map3

map4
  : (a -> b -> c -> d -> result)
  -> Matrix a -> Matrix b -> Matrix c -> Matrix d -> Matrix result
map4 = Array.map4 << Array.map4

map5
  : (a -> b -> c -> d -> e -> result)
  -> Matrix a -> Matrix b -> Matrix c -> Matrix d -> Matrix e -> Matrix result
map5 = Array.map5 << Array.map5
