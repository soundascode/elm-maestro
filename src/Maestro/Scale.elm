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
import Maestro.PitchClass exposing (Tone)
import String exposing (toLower)


{-| Scale represents a list of tones composing it
-}
type alias Scale =
    List Tone


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


{-| Given a Tone and a Mode, generates the tones
composing a scale.

    (==)
        scale
        (newTone C Natural)
        Major
        [ { class = C, adjustment = Natural }
        , { class = D, adjustment = Natural }
        , { class = E, adjustment = Natural }
        , { class = F, adjustment = Natural }
        , { class = G, adjustment = Natural }
        , { class = A, adjustment = Natural }
        , { class = B, adjustment = Natural }
        ]

-}
scale : Tone -> Mode -> List Tone
scale tone mode =
    let
        placeholderNote =
            newNote tone.class tone.adjustment 3
    in
    List.map (\i -> (addInterval placeholderNote i).tone) (modeToIntervals mode)


{-| scaleFrom generates the notes composing a scale, according
to a provided note (tone + octave) and mode
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
