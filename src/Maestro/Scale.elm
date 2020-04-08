module Maestro.Scale
    exposing
        ( Scale(..)
        , pitches
        , toString
        , fromString
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
        )
import Maestro.Note exposing (newNote)
import Maestro.Pitch exposing (Pitch)
import String exposing (toLower)


{-| Scale represents a list of pitches composing it
-}
type Scale
    = Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian


{-| Given a Pitch and a Scale, returns the pitches composing it.

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
pitches : Pitch -> Scale -> List Pitch
pitches pitch mode =
    let
        placeholderNote =
            newNote pitch.class pitch.accidental 3
    in
        List.map (\i -> (addInterval placeholderNote i).pitch) (toIntervals mode)


toIntervals : Scale -> List Interval
toIntervals mode =
    case mode of
        Ionian ->
            [ PerfectUnison
            , MajorSecond
            , MajorThird
            , PerfectFourth
            , PerfectFifth
            , MajorSixth
            , MajorSeventh
            ]

        Dorian ->
            [ PerfectUnison
            , MajorSecond
            , MinorThird
            , PerfectFourth
            , PerfectFifth
            , MajorSixth
            , MinorSeventh
            ]

        Phrygian ->
            [ PerfectUnison
            , MinorSecond
            , MinorThird
            , PerfectFourth
            , PerfectFifth
            , MinorSixth
            , MinorSeventh
            ]

        Lydian ->
            [ PerfectUnison
            , MajorSecond
            , MajorThird
            , AugmentedFourth
            , PerfectFifth
            , MajorSixth
            , MajorSeventh
            ]

        Mixolydian ->
            [ PerfectUnison
            , MajorSecond
            , MajorThird
            , PerfectFourth
            , PerfectFifth
            , MajorSixth
            , MinorSeventh
            ]

        Aeolian ->
            [ PerfectUnison
            , MajorSecond
            , MinorThird
            , PerfectFourth
            , PerfectFifth
            , MinorSixth
            , MinorSeventh
            ]

        Locrian ->
            [ PerfectUnison
            , MinorSecond
            , MinorThird
            , PerfectFourth
            , DiminishedFifth
            , MinorSixth
            , MinorSeventh
            ]


{-| toString converts a scale to its name as a String
-}
toString : Scale -> String
toString s =
    case s of
        Ionian ->
            "ionian"

        Dorian ->
            "dorian"

        Phrygian ->
            "phryigian"

        Lydian ->
            "lydian"

        Mixolydian ->
            "mixolydian"

        Aeolian ->
            "aeolian"

        Locrian ->
            "locrian"


{-| fromString parses a scale from a string
-}
fromString : String -> Maybe Scale
fromString mode =
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
