module Maestro.Chord exposing (Chord, Quality(..), chord)

{-| This module provides types and functions to create and
manipulate chords.

# Types
@docs Chord, Quality

# Chord generation
@docs chord

-}

import Maestro.Tone exposing (Tone)
import Maestro.Note exposing (newNote)
import Maestro.Interval exposing (Interval(..), addInterval)


{-| Chord represents a list of tones composing it
-}
type alias Chord =
    List Tone


{-| Quality represents the quality of a scale.
-}
type Quality
    = MajorTriad
    | MinorTriad


{-| Given a Tone and a Quality, generates the tones
composing a chord.

    (==)
      chord (newTone C Natural) MajorTriad
      [ { key = C, adjustment = Natural }
      , { key = E, adjustment = Natural }
      , { key = G, adjustment = Natural }
      ]
-}
chord : Tone -> Quality -> Chord
chord tone quality =
    let
        placeholderNote =
            newNote tone.key tone.adjustment 3
    in
        List.map (\i -> (addInterval placeholderNote i).tone) (qualityToIntervals quality)


qualityToIntervals : Quality -> List Interval
qualityToIntervals quality =
    case quality of
        MajorTriad ->
            majorTriad

        MinorTriad ->
            minorTriad


majorTriad : List Interval
majorTriad =
    [ PerfectUnison
    , MajorThird
    , PerfectFifth
    ]


minorTriad : List Interval
minorTriad =
    [ PerfectUnison
    , MinorThird
    , PerfectFifth
    ]
