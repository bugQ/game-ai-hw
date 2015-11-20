module StateMachine where

import Array exposing (Array)
import Maybe exposing (andThen)
import ArrayToList exposing (indices)


--- TYPES ---

-- Int is used to denote state, but anything outside
-- a StateMachine's states list range is a dead end.
type alias State = Int

type alias Condition a = a -> Bool

type alias Action a = a -> a

type alias Transition a = (Condition a, Action a, State)

-- This FSM keeps track of additional arbitrary info alongside the State.
-- For example, physics data such as position, velocity, acceleration.
-- The first transition in each state's transition list has highest priority.
type alias StateMachine a =
 { state : State
 , info : a
 , states : Array String
 , rules : Array (List (Transition a))
 }


--- CREATION ---

-- starting state assumed first element
new : List String -> a -> StateMachine a
new states info =
 { state = 0
 , info = info
 , states = Array.fromList states
 , rules = Array.repeat (List.length states) []
 }

-- add a transition with lower priority than existing transitions
-- this is slow but it should happen during startup only
addRule : (String, Condition a, Action a, String) -> StateMachine a -> StateMachine a
addRule (src, cond, act, dst) machine =
  case indices src machine.states of
    [] -> machine
    state :: _ -> case indices dst machine.states of
      [] -> machine
      next :: _ -> case Array.get state machine.rules of
        Nothing -> machine
        Just stateRules -> { machine | rules =
          Array.set state (stateRules ++ [(cond, act, next)]) machine.rules }


--- USAGE ---

-- just update the additional internal info (such as physics data)
apprise : a -> StateMachine a -> StateMachine a
apprise info machine = { machine | info = info }

-- conditionally make a transition based on internal info
step : StateMachine a -> StateMachine a
step machine = case Array.get machine.state machine.rules of
  Nothing -> machine
  Just stateRules ->
    case List.filter (\(cond, _, _) -> cond machine.info) stateRules of
      [] -> machine
      (_, act, next) :: _ -> { machine | state = next, info = act machine.info }

-- conditionally make a transition based on external info
update : a -> StateMachine a -> StateMachine a
update info = apprise info >> step
