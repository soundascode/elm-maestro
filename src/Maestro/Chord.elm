module Maestro.Chord exposing
    ( Chord, Quality(..)
    , chord
    , inversion, qualityToString
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
import Maestro.PitchClass exposing (Pitch)


{-| Chord represents a list of pitches composing it
-}
type alias Chord =
    List Pitch


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


{-| Given a Pitch and a Quality, generates the pitches
composing a chord.

    (==)
        chord
        (newPitch C Natural)
        Major
        [ { class = C, adjustment = Natural }
        , { class = E, adjustment = Natural }
        , { class = G, adjustment = Natural }
        ]

-}
chord : Pitch -> Quality -> Chord
chord pitch quality =
    let
        placeholderNote =
            newNote pitch.class pitch.adjustment 3
    in
    List.map (\i -> (addInterval placeholderNote i).pitch) (qualityToIntervals quality)


inversion : Int -> Chord -> Chord
inversion i c =
    case i of
        0 ->
            c

        _ ->
            rotate (i - 1) c


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


qualityToString : Quality -> String
qualityToString q =
    case q of
        MajorTriad ->
            "M"

        MinorTriad ->
            "m"

        AugmentedTriad ->
            "+"

        DiminishedTriad ->
            "-"

        Seventh ->
            "7"

        MajorSeventh ->
            "maj7"

        MinorMajorSeventh ->
            "mM7"

        MinorSeventh ->
            "7"

        AugmentedMajorSeventh ->
            "+M7"

        AugmentedSeventh ->
            "+7"

        HalfDiminishedSeventh ->
            "ø7"

        DiminishedSeventh ->
            "-7"

        SeventhFlatFive ->
            "7♭5"


pitchesCount : Quality -> Int
pitchesCount q =
    case q of
        MajorTriad ->
            3

        MinorTriad ->
            3

        AugmentedTriad ->
            3

        DiminishedTriad ->
            3

        Seventh ->
            4

        MajorSeventh ->
            4

        MinorMajorSeventh ->
            4

        MinorSeventh ->
            4

        AugmentedMajorSeventh ->
            4

        AugmentedSeventh ->
            4

        HalfDiminishedSeventh ->
            4

        DiminishedSeventh ->
            4

        SeventhFlatFive ->
            4


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
