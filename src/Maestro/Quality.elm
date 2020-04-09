module Maestro.Quality exposing
    ( Quality(..)
    , toIntervals
    , toString
    )

import Maestro.Interval exposing (Interval(..))


{-| ChordQuality represents the quality of a chord.
-}
type Quality
    = Major
    | Minor
    | Augmented
    | Diminished
    | HalfDiminished
    | Dominant


toIntervals : Quality -> List Interval
toIntervals quality =
    case quality of
        Major ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.PerfectFifth
            ]

        Minor ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MinorThird
            , Maestro.Interval.PerfectFifth
            ]

        Augmented ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.AugmentedFifth
            ]

        Diminished ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MinorThird
            , Maestro.Interval.DiminishedFifth
            ]

        HalfDiminished ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MinorThird
            , Maestro.Interval.DiminishedFifth
            , Maestro.Interval.MinorSeventh
            ]

        Dominant ->
            [ Maestro.Interval.PerfectUnison
            , Maestro.Interval.MajorThird
            , Maestro.Interval.PerfectFifth
            , Maestro.Interval.MinorSeventh
            ]


toString : Quality -> String
toString q =
    case q of
        Major ->
            "M"

        Minor ->
            "m"

        Augmented ->
            "+"

        Diminished ->
            "o"

        HalfDiminished ->
            "ø"

        Dominant ->
            "dom"
