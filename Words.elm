module Words (User, nletters, nvowels, initUser, delChar, reFill,
              upScore, reroll, addChars, shuffle, ravi,
              multiIntersperse) where

import Char (..)
import List
import Random as R
import Debug

nletters = 7
nvowels  = 2
-- ravi = False
ravi = True

vowels = List.map fromCode [65, 69, 73, 79, 85]

--- MODULE FUNCTIONS ---
type alias User =
  { chars : List Char
  , score : Int
  , genSeed : R.Seed
  , randGen : R.Generator Int
  }

initUser : Int -> User
initUser sd =
  let
    randGen' = R.list (nletters - nvowels) (R.int 65 90)
    initSeed =
      case sd of
        42 -> R.initialSeed 42
        _  -> R.initialSeed sd
    randList = R.generate randGen' initSeed
  in
    { chars = snd (balance nvowels (List.map fromCode (fst randList)) initSeed)
    , score = 0
    , genSeed = snd randList
    , randGen = R.int 65 90
    }

addChars : List Char -> User -> User
addChars cs usr =
  let
    nuchars = List.filter (\x -> x /= '0') usr.chars
  in
    updateChars (List.append cs nuchars) usr

delChar : Char -> List Char -> User -> User
delChar c cs usr =
  let
    upC = toCode c
  in
    case cs of
      a::b -> if toCode a == upC
              then updateChars ('0'::b) usr
              else updateChars (a::((delChar c b usr).chars)) usr
      _    -> usr

reFill : User -> User
reFill usr =
  let
    usrc' = (List.filter (\x -> x /= '0') usr.chars)
    rg = R.list (nletters - List.length usrc') usr.randGen
    (rlist, rseed) = R.generate rg usr.genSeed
    nusr = addChars (List.map fromCode rlist) (updateSeed rseed usr)
    filtered = Debug.watch "Consonants" <| List.filter (\x -> not <| List.member x vowels) nusr.chars
    newvows = nletters - (List.length filtered)
    bal = if | newvows < 2 -> balance 2 (List.drop (2 - newvows) filtered) rseed
             | otherwise   -> (rseed, nusr.chars)
  in
    updateChars (snd bal) (updateSeed (fst bal) nusr)

upScore : Int -> User -> User
upScore = updateScore

reroll : User -> User
reroll usr =
  let
    usr' =
      { chars = []
      , score = usr.score
      , genSeed = usr.genSeed
      , randGen = usr.randGen
      }
  in
    reFill usr'

shuffle : User -> User
shuffle usr =
  let
    len = nletters//2
    cs1 = List.take len usr.chars
    cs2 = List.drop len usr.chars
  in
    updateChars (multiIntersperse cs1 cs2) usr

--- HELPERS ---
multiIntersperse : List a -> List a -> List a
multiIntersperse as1 as2 =
  case (as1, as2) of
    (h1::t1, h2::t2) -> List.append (multiIntersperse t1 t2) [h2, h1]
    ([], _)          -> as2
    (_, [])          -> as1

randVowels : Int -> R.Seed -> (R.Seed, List Char)
randVowels n sd =
  let
    rg = R.list n (R.int 1 5)
    (c, sd') = R.generate rg sd
    ntov x =
      if | x==1 -> 65 | x==2 -> 69 | x==3 -> 73 | x==4 -> 79 | x==5 -> 85
  in
    (sd', List.map fromCode (List.map ntov c))

balance : Int -> List Char -> R.Seed -> (R.Seed, List Char)
balance n cs sd =
    let rv = randVowels n sd in
    (fst rv, List.append cs (snd rv))

updateChars : List Char -> User -> User
updateChars cs usr =
  { chars = cs
  , score = usr.score
  , genSeed = usr.genSeed
  , randGen = usr.randGen
  }

updateScore : Int -> User -> User
updateScore n usr =
  let nusr = reFill usr in
  { chars = nusr.chars
  , score = usr.score + n
  , genSeed = nusr.genSeed
  , randGen = nusr.randGen
  }

updateSeed : R.Seed -> User -> User
updateSeed sd usr =
  { chars = usr.chars
  , score = usr.score
  , genSeed = sd
  , randGen = usr.randGen
  }
