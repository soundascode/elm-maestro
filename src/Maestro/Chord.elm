module Maestro.Chord exposing
    ( Chord
    , inversion, newChord, pitches
    )

{-| This module provides types and functions to create and
manipulate chords.


# Types

@docs Chord, Quality


# Chord generation

@docs chord, inversion1

-}

import ListUtils exposing (rotate)
import Maestro.Interval exposing (Interval(..), addInterval)
import Maestro.Note exposing (newNote)
import Maestro.Pitch exposing (Pitch)
import Maestro.Quality exposing (Quality)


type alias Chord =
    { root : Pitch
    , quality : Quality
    }


newChord : Pitch -> Quality -> Chord
newChord p q =
    { root = p, quality = q }


{-| Given a Chord, returns the pitches composing it.

    (==)
        pitches
        (newChord (newPitch C Natural) Major)
        [ { class = C, accidental = Natural }
        , { class = E, accidental = Natural }
        , { class = G, accidental = Natural }
        ]

-}
pitches : Chord -> List Pitch
pitches c =
    let
        placeholderNote =
            newNote c.root.class c.root.accidental 3
    in
    List.map (\i -> (addInterval placeholderNote i).pitch) (Maestro.Quality.toIntervals c.quality)


{-| Chord represents a list of pitches composing it
-}
inversion : Int -> Chord -> List Pitch
inversion i c =
    case i of
        0 ->
            pitches c

        _ ->
            rotate (i - 1) <| pitches c
