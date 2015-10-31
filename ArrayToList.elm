module ArrayToList where

import Array exposing (Array, foldl, get)
import Maybe exposing (andThen)

filter : (a -> Bool) -> Array a -> List a
filter f aa = foldl (\a aa -> if f a then a :: aa else aa) [] aa

map : (a -> b) -> Array a -> List b
map f aa = foldl (\a bb -> f a :: bb) [] aa

-- map2 : (a -> b -> c) -> Array a -> Array b -> List c
-- map2 f aa bb = indexedFilterMap (\i a -> get i bb `andThen` f a) aa

filterMap : (a -> Maybe b) -> Array a -> List b
filterMap f aa = foldl (\a bb -> case f a of
  Just b -> b :: bb
  Nothing -> bb) [] aa

indexedMap : (Int -> a -> b) -> Array a -> List b
indexedMap f aa = foldl (\a (i, bb) -> (i + 1, f i a :: bb)) (0, []) aa |> snd

indexedFilterMap : (Int -> a -> Maybe b) -> Array a -> List b
indexedFilterMap f aa = Array.foldl (\a (i, bb) -> case f i a of
  Just b -> (i + 1, b :: bb)
  Nothing -> (i + 1, bb)) (0, []) aa |> snd

indices : a -> Array a -> List Int
indices x aa = indexedFilterMap (\i a -> if a == x then Just i else Nothing) aa
