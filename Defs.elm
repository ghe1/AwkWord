module Defs where

import Words (..)

import Signal
import Time as T
import Graphics.Input.Field as F
import Keyboard (KeyCode)

--- DEFINITIONS ---
type alias Point = {x : Float, y : Float}

type alias State = (User, RunState)

type Gameplay
  = Done T.Time
  | Tick
  | Start String
  | Again
  | Pause
  | Play
  | Enter String
  | Key KeyCode
  | Shuffle
  | Reroll

type RunState
  = Begin
  | Running T.Time (List Char)
  | Moving T.Time (List Char) (List Char)
  | Shuffling T.Time (List Char) (List Char) --time, entered, user's letters
  | Paused T.Time (List Char)
  | End T.Time

ptToTuple : Point -> (Float, Float)
ptToTuple pt = (pt.x, pt.y)

tupleToPt : (Float, Float) -> Point
tupleToPt (a, b) = {x=a, y=b}

initState : State
initState = (initUser 42, Begin)

gameplay : Signal.Channel Gameplay
gameplay = Signal.channel (Done 0)

seed : Signal.Channel F.Content
seed = Signal.channel F.noContent
