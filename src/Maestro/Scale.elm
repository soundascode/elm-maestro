module Maestro.Scale exposing
    ( Scale, Mode(..)
    , scale
    , modeFromString
    , scaleFrom
    )

{-| This module provides types and functions to create and
manipulate scales.


# Types

@docs Scale, Mode


# Scales manipulation

@docs scale


# Helpers

@docs modeFromString

-}

import Maestro.Interval
    exposing
        ( Interval(..)
        , addInterval
        , aeolianIntervals
        , dorianIntervals
        , ionianIntervals
        , locrianIntervals
        , lydianIntervals
        , mixolydianIntervals
        , phrygianIntervals
        )
import Maestro.Note exposing (Note, newNote)
import Maestro.Pitch exposing (Pitch)
import String exposing (toLower)


{-| Scale represents a list of pitches composing it
-}
type alias Scale =
    List Pitch


{-| Mode represents the mode of a scale.
-}
type Mode
    = Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian


{-| Given a Pitch and a Mode, generates the pitches
composing a scale.

    (==)
        scale
        (newPitch C Natural)
        Major
        [ { class = C, accidental = Natural }
        , { class = D, accidental = Natural }
        , { class = E, accidental = Natural }
        , { class = F, accidental = Natural }
        , { class = G, accidental = Natural }
        , { class = A, accidental = Natural }
        , { class = B, accidental = Natural }
        ]

-}
scale : Pitch -> Mode -> List Pitch
scale pitch mode =
    let
        placeholderNote =
            newNote pitch.class pitch.accidental 3
    in
    List.map (\i -> (addInterval placeholderNote i).pitch) (modeToIntervals mode)


{-| scaleFrom generates the notes composing a scale, according
to a provided note (pitch + octave) and mode
-}
scaleFrom : Note -> Mode -> List Note
scaleFrom note mode =
    List.map (\i -> addInterval note i) (modeToIntervals mode)


modeToIntervals : Mode -> List Interval
modeToIntervals mode =
    case mode of
        Ionian ->
            ionianIntervals

        Dorian ->
            dorianIntervals

        Phrygian ->
            phrygianIntervals

        Lydian ->
            lydianIntervals

        Mixolydian ->
            mixolydianIntervals

        Aeolian ->
            aeolianIntervals

        Locrian ->
            locrianIntervals


{-| modeFromString parses a Mode from a String
-}
modeFromString : String -> Maybe Mode
modeFromString mode =
    case toLower mode of
        "major" ->
            Just Ionian

        "ionian" ->
            Just Ionian

        "minor" ->
            Just Aeolian

        "aeolian" ->
            Just Aeolian

        "dorian" ->
            Just Dorian

        "phrygian" ->
            Just Phrygian

        "lydian" ->
            Just Lydian

        "mixolydian" ->
            Just Mixolydian

        "locrian" ->
            Just Locrian

        _ ->
            Nothing
