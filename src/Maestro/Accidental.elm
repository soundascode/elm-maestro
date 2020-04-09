module Maestro.Accidental exposing
    ( Accidental(..)
    , fromString
    , fromSemitones
    , toString
    , toSemitones
    )

{-| Accidental represents an accidental applied to a key
-}

import String exposing (toLower)


type Accidental
    = Natural
    | Sharp
    | Flat
    | SharpSharp
    | FlatFlat


{-| toSemitones returns the numbers of semitones to apply to a
PitchClass when calculating its position.
-}
toSemitones : Accidental -> Int
toSemitones accidental =
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


{-| fromSemitones returns the accidental corresponding to a given
number of semitones
-}
fromSemitones : Int -> Accidental
fromSemitones value =
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


{-| fromString parses an accidental from a String
-}
fromString : String -> Maybe Accidental
fromString adj =
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


{-| toString parses an accidental from a String
-}
toString : Accidental -> String
toString adj =
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
