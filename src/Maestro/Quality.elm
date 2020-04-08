module Maestro.Quality exposing
    ( Quality(..)
    , toIntervals
    , toString
    )

import Maestro.Interval exposing (Interval(..))


{-| ChordQuality represents the quality of a chord.
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


toIntervals : Quality -> List Interval
toIntervals quality =
    case quality of
        MajorTriad ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.PerfectFifth
            ]

        MinorTriad ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MinorThird
            , Maestro.Interval.PerfectFifth
            ]

        AugmentedTriad ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.AugmentedFifth
            ]

        DiminishedTriad ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MinorThird
            , Maestro.Interval.DiminishedFifth
            ]

        Seventh ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.PerfectFifth
            , Maestro.Interval.MinorSeventh
            ]

        MajorSeventh ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.PerfectFifth
            , Maestro.Interval.MajorSeventh
            ]

        MinorMajorSeventh ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MinorThird
            , Maestro.Interval.PerfectFifth
            , Maestro.Interval.MajorSeventh
            ]

        MinorSeventh ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MinorThird
            , Maestro.Interval.PerfectFifth
            , Maestro.Interval.MinorSeventh
            ]

        AugmentedMajorSeventh ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.AugmentedFifth
            , Maestro.Interval.MajorSeventh
            ]

        AugmentedSeventh ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.AugmentedFifth
            , Maestro.Interval.MinorSeventh
            ]

        HalfDiminishedSeventh ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MinorThird
            , Maestro.Interval.DiminishedFifth
            , Maestro.Interval.MinorSeventh
            ]

        DiminishedSeventh ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MinorThird
            , Maestro.Interval.DiminishedFifth
            , Maestro.Interval.DiminishedSeventh
            ]

        SeventhFlatFive ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.DiminishedFifth
            , Maestro.Interval.MinorSeventh
            ]


toString : Quality -> String
toString q =
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
