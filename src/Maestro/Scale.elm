module Maestro.Scale exposing (Scale, Mode(..), scale, modeFromString)

{-| This module provides types and functions to create and
manipulate scales.

# Types
@docs Scale, Mode

# Scales manipulation
@docs scale

# Helpers
@docs modeFromString
-}

import Maestro.Tone exposing (Tone, Key(..), Adjustment(..), newTone)
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


{-| chromaticScale returns the chromatic scale tones starting at C.
The adjusted tones will be sharped or flatted according to the provided Adjustment.
This is mostly a helper function.
-}
chromaticScale : Adjustment -> List Tone
chromaticScale adj =
    let
        sharpedTones =
            [ newTone C Natural
            , newTone C Sharp
            , newTone D Natural
            , newTone D Sharp
            , newTone E Natural
            , newTone F Natural
            , newTone F Sharp
            , newTone G Natural
            , newTone G Sharp
            , newTone A Natural
            , newTone A Sharp
            , newTone B Natural
            ]

        flattedTones =
            [ newTone C Natural
            , newTone D Flat
            , newTone D Natural
            , newTone E Flat
            , newTone E Natural
            , newTone F Natural
            , newTone G Flat
            , newTone G Natural
            , newTone A Flat
            , newTone A Natural
            , newTone B Flat
            , newTone B Natural
            ]
    in
        case adj of
            Natural ->
                sharpedTones

            Sharp ->
                sharpedTones

            Flat ->
                flattedTones

            SharpSharp ->
                sharpedTones

            FlatFlat ->
                flattedTones


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
