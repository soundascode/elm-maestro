module Maestro.Scale exposing (Scale, Mode(..), scale, scaleFrom, modeFromString)

{-| This module provides types and functions to create and
manipulate scales.

# Types
@docs Scale, Mode

# Scales manipulation
@docs scale

# Helpers
@docs modeFromString
-}

import Maestro.Tone exposing (Tone)
import Maestro.Note exposing (Note, newNote)
import Maestro.Interval
    exposing
        ( Interval(..)
        , addInterval
        , ionianIntervals
        , dorianIntervals
        , phrygianIntervals
        , lydianIntervals
        , mixolydianIntervals
        , aeolianIntervals
        , locrianIntervals
        )
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
      scale (newTone C Natural) Major
      [ { key = C, adjustment = Natural }
      , { key = D, adjustment = Natural }
      , { key = E, adjustment = Natural }
      , { key = F, adjustment = Natural }
      , { key = G, adjustment = Natural }
      , { key = A, adjustment = Natural }
      , { key = B, adjustment = Natural }
      ]
-}
scale : Tone -> Mode -> List Tone
scale tone mode =
    let
        placeholderNote =
            newNote tone.key tone.adjustment 3
    in
        List.map (\i -> (addInterval placeholderNote i).tone) (modeToIntervals mode)


{-| scaleFrom generates the notes composing a scale, according
to a provided note (tone + octave) and mode
-}
scaleFrom : Note -> Mode -> List Note
scaleFrom note mode =
    List.map (\i -> (addInterval note i)) (modeToIntervals mode)


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
