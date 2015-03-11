module Game where

import Words (..)
import Defs (..)
import Viewing (..)
import ENDictionary (memberDict)

import Keyboard (KeyCode, lastPressed)
import Char (fromCode)
import String
import List

import Graphics.Element as E
import Graphics.Input.Field as F
import Window
import Debug

import Random as R
import Time as T
import Signal as S
import Signal ((<~), (~))

--- MAIN ---
mergeWithTime : T.Time -> S.Signal Gameplay -> S.Signal ((T.Time, T.Time), Gameplay)
mergeWithTime t gp =
  let
    time = T.timestamp (T.every t)
  in
    S.mergeMany
      [ (\t -> (t, Tick)) <~ time
      , (\t m -> (t, m)) <~ S.sampleOn gp time ~ gp
      , (\t k -> (t, Key k)) <~ time ~ lastPressed
      ]

gpKey : S.Signal Gameplay -> S.Signal KeyCode -> S.Signal (Gameplay, KeyCode)
gpKey gp kc =
  (\t k -> (t, k)) <~ gp ~ kc

main : S.Signal E.Element
main = view_
        <~ (Window.dimensions)
        ~ (fst <~ (T.timestamp <| T.fps 60))
        ~ (fst <~ (T.timestamp <| gpKey (S.subscribe gameplay) lastPressed))
        ~ (S.foldp upstate initState
            (mergeWithTime T.second (S.subscribe gameplay)))
        ~ (S.subscribe gameplay)
        ~ (S.subscribe seed)

--- UPDATING ---
upstate : ((T.Time, T.Time), Gameplay) -> State -> State
upstate ((tmstmp, tm), gp) (u, rs) =
  case gp of
    Done t    -> (u, End t)

    Tick      ->
      case rs of
        Running 0 str      -> (Debug.watch "user" u, End 0)
        Running t str      -> (Debug.watch "user" u, Running (t - 1) str)
        Shuffling 0 str ou -> (Debug.watch "user" u, End 0)
        Shuffling t str ou -> (Debug.watch "user" u, Shuffling (t - 1) str ou)
        Moving 0 str ou    -> (Debug.watch "user" u, End 0)
        Moving t str ou    -> (Debug.watch "user" u, Moving (t - 1) str ou)
        _                  -> (Debug.watch "user" u, rs)

    Start str ->
      case String.toInt(str) of
        Ok n  -> (initUser n, Running 60 [])
        Err _ -> (initUser (truncate tmstmp), Running 60 [])

    Again     -> initState

    Pause     ->
      case rs of
        Running t str     -> (u, Paused t str)
        Shuffling t str _ -> (u, Paused t str)
        Moving t str _    -> (u, Paused t str)
        _                 -> (u, rs)

    Play      ->
      case rs of
        Paused t str -> (u, Running t str)
        _            -> (u, rs)

    Enter str ->
      let
        enterup t str =
          if (memberDict (String.toLower (String.fromList (List.reverse str))))
          then (reFill (upScore (10 * List.length str) u), Running t [])
          else (addChars str u, Running t [])
      in
        case rs of
          Running t str      -> enterup t str
          Shuffling t str ou -> enterup t str
          Moving t str _     -> enterup t str

    Key n     ->
      let
        keyup t str =
          if | n == 8  ->
              -- backspace \b
                (addChars (List.take 1 str) u, Running t (List.drop 1 str))
             | n == 13 ->
              -- enter \r
                if (memberDict (String.toLower (String.fromList (List.reverse str))))
                then (reFill (upScore (10 * List.length str) u), Running t [])
                else (addChars str u, Running t [])
             | n == 27 -> (u, Paused t str)
              -- Space
             | n == 32 -> (shuffle u, Shuffling t str u.chars)
              -- ESC
             | n >= 65 && n <= 90 && (List.member (fromCode n) u.chars) ->
              -- any letter
                ((delChar (fromCode n) u.chars u), Moving t ((fromCode n)::str) u.chars)
             | otherwise -> (u, Running t str)
              -- unknown
      in
        case rs of
          Running t str      -> keyup t str
          Shuffling t str ou -> keyup t str
          Moving t str ou    -> keyup t str
          Paused t str       ->
            if n == 32 then (u, Running t str) else (u, rs)
          End t              ->
            if n == 32 then (u, Begin) else (u, rs)
          Begin              ->
            if n == 32 then (initUser (truncate tmstmp), Running 60 []) else (u, rs)
          _                  -> (u, rs)

    Shuffle   ->
      case rs of
        Running t str     -> (shuffle u, Shuffling t str u.chars)
        Shuffling t str _ -> (shuffle u, Shuffling t str u.chars)
        Moving t str _    -> (shuffle u, Shuffling t str u.chars)
        _                 -> (u, rs)

    Reroll    ->
      case rs of
        Running t _     -> (reroll (upScore -20 u), Running t [])
        Shuffling t _ _ -> (reroll (upScore -20 u), Running t [])
        Moving t _ _    -> (reroll (upScore -20 u), Running t [])
        _               -> (u, rs)

--- GRAPHICS ---
view_ : (Int, Int) -> T.Time -> T.Time -> State
        -> Gameplay -> F.Content -> E.Element
view_ (w, h) fpstm clicktm st gp sd  =
  case st of
    (_, Begin)           -> view_begin (w, h) st gp sd
    (u, Moving _ _ _)    -> view_moving (w, h) fpstm clicktm st gp
    (u, Shuffling _ _ _) -> view_shuffle (w, h) fpstm clicktm st gp
    (u, Running t str)   -> view_running (w, h) st gp
    (u, Paused t str)    -> view_paused (w, h) st
    (u, End t)           -> view_end (w, h) st
