module Heap exposing (..)

type Heap comparable = Leaf | Node comparable (List (Heap comparable))

findmin : Heap comparable -> Maybe comparable
findmin heap = case heap of
  Leaf -> Nothing
  Node element _ -> Just element

merge : Heap comparable -> Heap comparable -> Heap comparable
merge heap1 heap2 = case heap1 of
  Leaf -> heap2
  Node element1 heaps1 -> case heap2 of
    Leaf -> heap1
    Node element2 heaps2 -> if (element1 < element2) then
      Node element1 (heap2 :: heaps1)
     else
      Node element2 (heap1 :: heaps2)

insert : comparable -> Heap comparable -> Heap comparable
insert element heap = merge (Node element []) heap

deletemin : Heap comparable -> Heap comparable
deletemin heap = case heap of
  Leaf -> Leaf
  Node _ heaps -> List.foldl merge Leaf heaps
