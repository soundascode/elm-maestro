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
    | AugmentedTriad
    | DiminishedTriad
    | Seventh
    | MajorSeventh
    | MinorMajorSeventh
    | MinorSeventh
    | AugmentedMajorSeventh
    | AugmentedSeventh
    | HalfDiminishedSeventh
    | DiminishedSeventh
    | SeventhFlatFive


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

        AugmentedTriad ->
            augmentedTriad

        DiminishedTriad ->
            diminishedTriad

        Seventh ->
            seventh

        MajorSeventh ->
            majorSeventh

        MinorMajorSeventh ->
            minorMajorSeventh

        MinorSeventh ->
            minorSeventh

        AugmentedMajorSeventh ->
            augmentedMajorSeventh

        AugmentedSeventh ->
            augmentedSeventh

        HalfDiminishedSeventh ->
            halfDiminishedSeventh

        DiminishedSeventh ->
            diminishedSeventh

        SeventhFlatFive ->
            seventhFlatFive


majorTriad : List Interval
majorTriad =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MajorThird
    , Maestro.Interval.PerfectFifth
    ]


minorTriad : List Interval
minorTriad =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MinorThird
    , Maestro.Interval.PerfectFifth
    ]


augmentedTriad : List Interval
augmentedTriad =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MajorThird
    , Maestro.Interval.AugmentedFifth
    ]


diminishedTriad : List Interval
diminishedTriad =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MinorThird
    , Maestro.Interval.DiminishedFifth
    ]


seventh : List Interval
seventh =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MajorThird
    , Maestro.Interval.PerfectFifth
    , Maestro.Interval.MinorSeventh
    ]


majorSeventh : List Interval
majorSeventh =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MajorThird
    , Maestro.Interval.PerfectFifth
    , Maestro.Interval.MajorSeventh
    ]


minorMajorSeventh : List Interval
minorMajorSeventh =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MinorThird
    , Maestro.Interval.PerfectFifth
    , Maestro.Interval.MajorSeventh
    ]


minorSeventh : List Interval
minorSeventh =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MinorThird
    , Maestro.Interval.PerfectFifth
    , Maestro.Interval.MinorSeventh
    ]


augmentedMajorSeventh : List Interval
augmentedMajorSeventh =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MajorThird
    , Maestro.Interval.AugmentedFifth
    , Maestro.Interval.MajorSeventh
    ]


augmentedSeventh : List Interval
augmentedSeventh =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MajorThird
    , Maestro.Interval.AugmentedFifth
    , Maestro.Interval.MinorSeventh
    ]


halfDiminishedSeventh : List Interval
halfDiminishedSeventh =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MinorThird
    , Maestro.Interval.DiminishedFifth
    , Maestro.Interval.MinorSeventh
    ]


diminishedSeventh : List Interval
diminishedSeventh =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MinorThird
    , Maestro.Interval.DiminishedFifth
    , Maestro.Interval.DiminishedSeventh
    ]


seventhFlatFive : List Interval
seventhFlatFive =
    [ Maestro.Interval.PerfectUnison
    , Maestro.Interval.MajorThird
    , Maestro.Interval.DiminishedFifth
    , Maestro.Interval.MinorSeventh
    ]
