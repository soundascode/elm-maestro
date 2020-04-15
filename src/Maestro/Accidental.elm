module Maestro.Accidental
    exposing
        ( Accidental(..)
        , fromString
        , fromSemitones
        , toString
        , toSemitones
        )

{-| This module provides abstractions and helpers to represent and manipulate
accidentals. An accidental is an alteration applied to a pitch. Applying an accidental to a pitch adds or substracts a predifined amount of semitones to it.
You're probably familiar with them already as they are represented with the ♯, ♭ and ♮ symbols in music notation.

# Definition
@docs Accidental

# Conversion
@docs toSemitones, toString

# Parsing
@docs fromSemitones, fromString
-}

import String exposing (toLower)


{-| Accidental represents an accidental applied to a pitch class in order to form a pitch.
-}
type Accidental
    = Natural
      -- // TODO: natural should cancel accidental, need a None type
    | Sharp
    | Flat
    | SharpSharp
      -- //TODO: rename to DoubleSharp
    | FlatFlat



-- //TODO: rename to DoubleFlat


{-| Converts an accidental to a number of semitones it applies to a pitch class.
Flat accidentals will return a negative value when Sharp ones will return a positive value.

    toSemitones Flat == -1
    toSemitones Sharp == 1
    toSemitones Natural == 0
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


{-| Converts an accidental into its string represenation

    toString Sharp == "♯"
    toString Flat == "♭"
-}
toString : Accidental -> String
toString adj =
    case adj of
        Natural ->
            ""

        Sharp ->
            "♯"

        SharpSharp ->
            "𝄪"

        Flat ->
            "♭"

        FlatFlat ->
            "♭♭"


{-| Interprets a given number of semitones as an Accidental.

    fromSemitones -1 == Flat
    fromSemitones 1 == Sharp
    fromSemitones 0 == Natural
-}
fromSemitones : Int -> Accidental
fromSemitones value =
    if value == -2 then  -- //TODO: should return a Maybe or Result
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


{-| Parses an accidental from a string

    fromString "♮" == Natural
    fromString "♯" == Sharp
    fromString "♭" == Flat
-}
fromString : String -> Maybe Accidental
fromString adj =
    case toLower adj of
        "" ->
            Just Natural

        " ♮" ->
            Just Natural

        "natural" ->
            Just Natural

        "♯" ->
            Just Sharp

        "sharp" ->
            Just Sharp

        "♭" ->
            Just Flat

        "flat" ->
            Just Flat

        "𝄪" ->
            Just SharpSharp

        "𝄫" ->
            Just FlatFlat

        _ ->
            Nothing
