module Maestro.Accidental exposing (..)

{-| Accidental represents an accidental applied to a key
-}

import String exposing (toLower)


type Accidental
    = Natural
    | Sharp
    | Flat
    | SharpSharp
    | FlatFlat


{-| accidentalToValue returns the numbers of semipitches to apply to a
PitchClass when calculating its position.
-}
accidentalToValue : Accidental -> Int
accidentalToValue accidental =
    case accidental of
        Flat ->
            -1

        FlatFlat ->
            -2

        Natural ->
            0

        Sharp ->
            1

        SharpSharp ->
            2


{-| accidentalFromValue returns the accidental corresponding to a given
number of semipitches
-}
accidentalFromValue : Int -> Accidental
accidentalFromValue value =
    if value == -2 then
        FlatFlat

    else if value == -1 then
        Flat

    else if value == 0 then
        Natural

    else if value == 1 then
        Sharp

    else if value == 2 then
        SharpSharp

    else
        Natural


{-| accidentalFromString parses an accidental from a String
-}
accidentalFromString : String -> Maybe Accidental
accidentalFromString adj =
    case toLower adj of
        "" ->
            Just Natural

        "natural" ->
            Just Natural

        "#" ->
            Just Sharp

        "sharp" ->
            Just Sharp

        "b" ->
            Just Flat

        "flat" ->
            Just Flat

        _ ->
            Nothing


{-| accidentalFromString parses an accidental from a String
-}
accidentalToString : Accidental -> String
accidentalToString adj =
    case adj of
        Natural ->
            ""

        Sharp ->
            "♯"

        SharpSharp ->
            "♯♯"

        Flat ->
            "♭"

        FlatFlat ->
            "♭♭"
