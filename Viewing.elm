module Viewing ( view_end, view_begin, view_paused
               , view_running, view_shuffle, view_moving) where

import Words (..)
import Defs (..)

import Char (fromCode, toCode)
import Keyboard (KeyCode)
import Signal
import String as S
import List ((::))
import List

import Time (Time, fps, second, millisecond, timestamp)
import Easing (ease, easeInQuad, point2d)

import Graphics.Collage as C
import Graphics.Element as E
import Graphics.Input as I
import Graphics.Input.Field as F
import Color (..)
import Text (..)

animateMed : Point -> Point -> Time -> Point
animateMed start end = ease easeInQuad point2d start end second

animateFast : Point -> Point -> Time -> Point
animateFast start end = ease easeInQuad point2d start end (200 * millisecond)

gameColor : Color
gameColor = lightBlue

--- RAVI MODE ---
ravimg : Char -> E.Element
ravimg c =
  (E.layers
    <| [ E.image 60 120 ("imgs/ravi.png ")
       , E.opacity 0.6 <| E.image 60 120 ("imgs/alpha/"++(S.fromChar c)++".png")
       ])
  |> I.clickable (Signal.send gameplay (Key (toCode c)))

--- REGULAR MODE ---
toimg : Char -> E.Element
toimg c =
  if | ravi && c /= '0' -> ravimg c
     | c == '0'  -> E.spacer 60 120
     | otherwise -> E.image 60 120 ("imgs/alpha/"++(S.fromChar c)++".png")
                      |> I.clickable (Signal.send gameplay (Key (toCode c)))

--- BUTTON PRETTYFIERS ---
mkBtn : Color -> String -> E.Element
mkBtn col t =
  let
    lineStyle =
      let ls = C.defaultLine in
      { ls | color <- darkCharcoal,
             width <- 10 }
    btnW = 200
    btnH = 60
  in
    C.collage btnW btnH
    [ C.filled col (C.rect btnW btnH)
    , C.outlined lineStyle (C.rect btnW btnH)
    , fromString t |> height 30 |> centered |> C.toForm
    ]

btnMkr : Signal.Message -> String -> E.Element
btnMkr sig str =
  I.customButton sig (mkBtn orange str) (mkBtn red str) (mkBtn yellow str)

--- VIEW BEGIN ---
beginButtons : F.Content -> E.Element
beginButtons content =
  btnMkr (Signal.send gameplay (Start (content.string))) "Start"

seedField : F.Content -> E.Element
seedField content =
 E.flow E.right
   <| [ F.field F.defaultStyle (Signal.send seed) "Seed" content
      ]

view_begin : (Int, Int) -> State -> Gameplay -> F.Content -> E.Element
view_begin (w, h) (u, Begin) gp ct =
  C.collage (w - 1) (h - 1)
    [ C.toForm (view_running (w, h) (u, Running 0 []) gp)
    , C.alpha 0.95 <| C.filled gameColor <| C.rect (toFloat w) (toFloat h)
    , C.moveY (toFloat (h//4)) <| C.toForm (E.image 600 200 "imgs/title.png")
    , C.moveY -20 <| C.toForm (beginButtons ct)
    , C.toForm (seedField ct) |> C.move (toFloat (w//2), toFloat (h//2)) |> C.move (-105, -20)
    ]

--- VIEW SHUFFLING ---
imap : Time -> Time -> List Int -> Int -> C.Form -> C.Form
imap fpstm clicktm ls i f =
  let
    xy = ptToTuple (animateMed (tupleToPt (-240.0 + (toFloat i)*80.0, 200.0))
         (tupleToPt (-240 + (toFloat (List.head (List.drop i ls)))*80.0, 200.0))
         (fpstm - clicktm))
  in
    C.move xy f

charForms_sf : Time -> Time -> List Char -> List C.Form
charForms_sf fpstm clicktm old_cs =
  let
    old_es = List.map C.toForm (List.map toimg old_cs)
    ls' = [6,4,2,5,3,1,0] -- Shuffled order
  in
    List.indexedMap (imap fpstm clicktm ls') old_es

view_shuffle : (Int, Int) -> Time -> Time -> State -> Gameplay -> E.Element
view_shuffle (w, h) fpstm clicktm (u, Shuffling t str ou) gp =
  let
    tcolor = if t < 10 then red else black
    tolist = charForms_sf fpstm clicktm ou
  in
  C.collage (w-1) (h-1)
    <| List.append
      [ C.filled gameColor <| C.rect (toFloat w) (toFloat h)
      , C.move (toFloat (-w//2) + 100, toFloat (h//2) - 50) <| C.toForm (E.image 210 70 "imgs/title.png")
      -- , C.moveY (toFloat (h//2) - 50) <| C.toForm (asText (u, Running t str))
      , C.toForm (runButtons (Running t str))
      , C.move (-100, 70) <| C.toForm <| centered <| height 30 <| color tcolor <| fromString <| "Time: "++(toString t)
      , C.move (100, 70) <| C.toForm <| centered <| height 30 <| fromString <| "Score: "++(toString u.score)
      , C.toForm (render_chars (w, h) (List.reverse str)) |> C.moveY -200
      ]
      tolist

--- VIEW MOVING ---
rmap : Time -> Time -> Int -> Int -> C.Form -> C.Form
   -- Curr -> Start -> From -> To -> Form
rmap fpstm clicktm from to f =
  let
    xy = ptToTuple (animateFast (tupleToPt (-240.0 + (toFloat from)*80.0, 200.0))
         (tupleToPt (-240.0 + 80.0*(toFloat to), -200.0))
         (fpstm - clicktm))
  in
    C.move xy f

charForms_mv : Time -> Time -> Char -> Int -> List Char -> C.Form
charForms_mv fpstm clicktm ch endIndex chs =
  let
    findi c ls acc =
      case ls of
        a::ls' -> if c == a then acc else (findi c ls' acc+1)
        []     -> acc + 1
    startIndex = findi (toCode ch) (List.map toCode chs) 0
  in
    rmap fpstm clicktm startIndex endIndex (C.toForm <| toimg ch)

view_moving : (Int, Int) -> Time -> Time -> State -> Gameplay -> E.Element
view_moving (w, h) fpstm clicktm (u, Moving t str ou) gp =
 let
   tcolor = if t < 10 then red else black
   str' = (List.drop 1 str)
   animated = charForms_mv fpstm clicktm (List.head str) (List.length str') ou
 in
 C.collage (w-1) (h-1)
   [ C.filled gameColor <| C.rect (toFloat w) (toFloat h)
   , C.move (toFloat (-w//2) + 100, toFloat (h//2) - 50) <| C.toForm (E.image 210 70 "imgs/title.png")
   , C.toForm (render_chars (w, h) u.chars) |> C.moveY 200
   , C.toForm (runButtons (Running t str))
   , C.move (-100, 70) <| C.toForm <| centered <| height 30 <| color tcolor <| fromString <| "Time: "++(toString t)
   , C.move (100, 70) <| C.toForm <| centered <| height 30 <| fromString <| "Score: "++(toString u.score)
   , C.toForm (render_chars (w, h) (List.reverse str')) |> C.moveY -200
   , animated
   ]

--- VIEW RUNNING ---
runButtons : RunState -> E.Element
runButtons (Running t cs) =
  let
    hspace = E.spacer 10 10
  in
    E.flow E.right
      <| List.intersperse hspace
        <| [ btnMkr (Signal.send gameplay Pause) "Pause"
           , btnMkr (Signal.send gameplay (Enter (S.fromList cs))) "Enter"
           , btnMkr (Signal.send gameplay Shuffle) "Shuffle"
           , btnMkr (Signal.send gameplay Reroll) "ReRoll"
           ]

view_running : (Int, Int) -> State -> Gameplay -> E.Element
view_running (w, h) (u, Running t str) gp =
  let
    tcolor = if t < 10 then red else black
  in
    C.collage (w-1) (h-1)
      [ C.filled gameColor <| C.rect (toFloat w) (toFloat h)
      , C.move (toFloat (-w//2) + 100, toFloat (h//2) - 50) <| C.toForm (E.image 210 70 "imgs/title.png")
      -- , C.moveY (toFloat (h//2) - 50) <| C.toForm (asText (u, Running t str))
      , C.toForm (runButtons (Running t str))
      , C.toForm (render_chars (w, h) u.chars) |> C.moveY 200
      , C.move (-100, 70) <| C.toForm <| centered <| height 30 <| color tcolor <| fromString <| "Time: "++(toString t)
      , C.move (100, 70) <| C.toForm <| centered <| height 30 <| fromString <| "Score: "++(toString u.score)
      , C.toForm (render_chars (w, h) (List.reverse str)) |> C.moveY -200
      ]

-- Static rendering
render_chars : (Int, Int) -> List Char -> E.Element
render_chars (w, h) cs =
  let
    es = List.map toimg cs
    es' = List.append es (List.repeat (nletters - (List.length es)) <| E.spacer 60 120)
    vspace = E.spacer 20 20
  in
    E.container (w - 5) 120 E.midTop
      <| E.flow E.right
        <| List.intersperse vspace es'

--- VIEW PAUSED ---
pauseButtons : RunState -> E.Element
pauseButtons (Paused t str) =
  let
    hspace = E.spacer 10 10
  in
    E.flow E.right
      <| List.intersperse hspace
        <| [ btnMkr (Signal.send gameplay Play) "Play"
           , btnMkr (Signal.send gameplay (Done t)) "End"
           ]

view_paused : (Int, Int) -> State -> E.Element
view_paused (w, h) (u, Paused t str) =
  C.collage (w-1) (h-1)
    [ C.toForm (view_running (w, h) (u, Running 0 []) (Done 0))
    , C.alpha 0.95 <| C.filled gameColor <| C.rect (toFloat w) (toFloat h)
    , C.move (toFloat (-w//2) + 100, toFloat (h//2) - 50) <| C.toForm (E.image 210 70 "imgs/title.png")
    , C.moveY 170 <| C.toForm <| centered <| height 50 <| fromString "Paused"
    , C.moveY 50 <| C.toForm <| centered <| height 30 <| fromString <| "Score: "++(toString u.score)
    , C.moveY -20 <| C.toForm (pauseButtons (Paused t str))
    ]

--- VIEW END ---
endButtons : E.Element
endButtons =
  let
    hspace = E.spacer 10 10
  in
    E.flow E.right
      <| List.intersperse hspace
        <| [ btnMkr (Signal.send gameplay Again) "Again"
           ]

view_end : (Int, Int) -> State -> E.Element
view_end (w, h) (u, End t) =
  let
    endmsg =
      if t == 0
        then centered <| height 50 <| fromString ("Time's up!")
        else centered <| height 50 <| fromString (":-( Try again!")
  in
    C.collage (w-1) (h-1)
      [ C.filled gameColor <| C.rect (toFloat w) (toFloat h)
      , C.move (toFloat (-w//2) + 100, toFloat (h//2) - 50) <| C.toForm (E.image 210 70 "imgs/title.png")
      , C.moveY 70 <| C.toForm <| centered <| height 30 <| fromString <| "Score: "++(toString u.score)
      , C.moveY 170 <| C.toForm endmsg
      , C.toForm (endButtons)
      ]
