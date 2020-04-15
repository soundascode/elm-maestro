module Maestro.PitchClass
    exposing
        ( PitchClass(..)
        , toSemitones
        , toDiatonicIndex
        , fromDiatonicIndex
        , toString
        )

{-| This module provides abstractions and helpers to represent and manipulate
pitch classes (C, D, E, F, G, A, and B pitches). It is used as the main building
block of Pitches and Notes.

N.B: For lack of a better abstraction, and name, although "PitchClass" is not the absolutely
correct name for the concept it's used, we settled on it.


# Types
@docs PitchClass

# Conversion
@docs toSemitones, toDiatonicIndex,

# Parsing
@docs toString, fromDiatonicIndex
-}


{-| PitchClass represents a Pitch class without accidental
-}
type PitchClass
    = C
    | D
    | E
    | F
    | G
    | A
    | B


{-| Translates a PitchClass to the number of semitones it represents.
Like on a piano, it is indexed on C (0).

    toSemitones C == 0
    toSemitones D == 2
    toSemitones B == 11
-}
toSemitones : PitchClass -> Int
toSemitones class =
    case class of
        C ->
            0

        D ->
            2

        E ->
            4

        F ->
            5

        G ->
            7

        A ->
            9

        B ->
            11


{-| Translates a PitchClass into a position on the diatonic scale (white notes on a piano).
Like on a piano, it is indexed on C (0).

    toDiatonicIndex C == 0
    toDiatonicIndex D == 1
    toDiatonicIndex B == 6
-}
toDiatonicIndex : PitchClass -> Int
toDiatonicIndex class =
    case class of
        C ->
            0

        D ->
            1

        E ->
            2

        F ->
            3

        G ->
            4

        A ->
            5

        B ->
            6


{-| Converts a PitchClass into its string represenation

    toString C == "C"
-}
toString : PitchClass -> String
toString k =
    case k of
        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"

        A ->
            "A"

        B ->
            "B"


{-| Attempts to interpret a diatonic index as a PitchClass.

    fromDiatonicIndex 0 == Just C
    fromDiatonicIndex 1 == Just D
    fromDiatonicIndex 6 == Just B
    fromDiatonicIndex 42 == Nothing
-}
fromDiatonicIndex : Int -> Maybe PitchClass
fromDiatonicIndex value =
    case value of
        0 ->
            Just C

        1 ->
            Just D

        2 ->
            Just E

        3 ->
            Just F

        4 ->
            Just G

        5 ->
            Just A

        6 ->
            Just B

        _ ->
            Nothing
