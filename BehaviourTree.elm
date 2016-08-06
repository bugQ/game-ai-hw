module BehaviourTree exposing (..)

type Result = Running | Success | Failure

type alias Routine a = a -> (a, Result)

type Behaviour a
  = Leaf (Routine a)
  | Repeat (Behaviour a) (Behaviour a)  -- original and current state
  | Sequence (List (Behaviour a))
  | Select (List (Behaviour a))

empty : Behaviour a
empty = Sequence []

repeat : Behaviour a -> Behaviour a
repeat b = Repeat b b

step : Behaviour a -> a -> (a, Result, Behaviour a)
step b_current data = case b_current of
  Leaf routine -> let (output, result) = routine data in
    (output, result, b_current)
  Repeat b_original b_sub -> let (output, result, b_next) = step b_sub data in
    case result of
      Running -> (output, Running, Repeat b_original b_next)
      Success -> (output, Running, Repeat b_original b_original)
      Failure -> (output, Success, Repeat b_original b_next)
  Sequence [] -> (data, Success, b_current)
  Sequence (b_sub :: []) -> let (output, result, b_next) = step b_sub data in
    case result of
      Running -> (output, Running, Sequence [b_next])
      Success -> (output, Success, Sequence [])
      Failure -> (output, Failure, Sequence [b_next])
  Sequence (b_sub :: bb) -> let (output, result, b_next) = step b_sub data in
    case result of
      Running -> (output, Running, Sequence (b_next :: bb))
      Success -> (output, Running, Sequence bb)
      Failure -> (output, Failure, Sequence (b_next :: bb))
  Select [] -> (data, Failure, b_current)
  Select (b_sub :: []) -> let (output, result, b_next) = step b_sub data in
    (output, result, Select [b_next])
  Select (b_sub :: bb) -> let (output, result, b_next) = step b_sub data in
    case result of
      Running -> (output, Running, Select (b_next :: bb))
      Success -> (output, Success, Select (b_next :: bb))
      Failure -> (output, Running, Select bb)
