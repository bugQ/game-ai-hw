module HeatMap exposing (..)

import Heap exposing (Heap)
import Array exposing (Array)
import Grid exposing (Grid, Point, manhattan, neighbors4)

type alias PropagateState = { frontier : Heap (Float, Int), heat : Array Float }

initHeat : Float -> Point -> Grid -> PropagateState
initHeat heat p0 grid = let i0 = Grid.index p0 grid in
  { frontier = Heap.insert (heat, i0) Heap.Leaf
  , heat = Array.initialize (Grid.size grid) (\i -> if i == i0 then heat else 0)
  }

propagate : Grid -> PropagateState -> PropagateState
propagate grid state = case Heap.findmin state.frontier of
  Nothing -> state
  Just (current_heat, i) ->
    List.foldl (\(next, _) s -> let
      j = Grid.index next grid
      neighbor_heat = Array.get j s.heat |> Maybe.withDefault (0)
      neighbor_cost = Grid.cost (Grid.get (Grid.deindex j grid) grid)
      new_heat = current_heat * 0.95 ^ neighbor_cost
     in
      if neighbor_heat < new_heat then
        { s
        | frontier = Heap.insert (new_heat, j) s.frontier
        , heat = Array.set j new_heat s.heat
        }
      else s
    ) { state | frontier = Heap.deletemin state.frontier }
    (neighbors4 (Grid.deindex i grid) grid)

heatNav : Grid -> Float -> Point -> Array Float
heatNav grid heat source = let
  pos_heat = abs heat
  init = initHeat heat source grid
  step = propagate grid
  return = .heat
  loop s = if s.frontier == Heap.Leaf then return s else loop (step s)
 in
  loop init |> if heat < 0 then Array.map negate else identity

heatProxInit : Grid -> Float -> Point -> Int -> Float
heatProxInit grid heat source i =
  heat * 0.7 ^ toFloat (manhattan source (Grid.deindex i grid))

heatProx : Grid -> Float -> Point -> Array Float
heatProx grid heat source = Array.initialize (Grid.size grid)
  (heatProxInit grid heat source)

heatProxMap : Grid -> Float -> Point -> Array Float -> Array Float
heatProxMap grid heat source = Array.indexedMap
  ((heatProxInit grid heat source) >> (+))
