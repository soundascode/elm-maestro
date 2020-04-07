module Maestro.PitchClass exposing
    ( PitchClass(..)
    , pitchClassToValue, pitchClassFromValue, pitchClassFromString
    , diatonicPitchClassValue, diatonicPitchClassFromValue
    , pitchClassToString
    )

{-| This module provides types and functions to manipulate musical pitches.
It allows you to represent pitches (pitches) like `C`, `C Sharp` and so on, as
well as helpers to represent these as numerical values.


# Types

@docs Pitch, PitchClass, Accidental


# Common Helpers

@docs newPitch, pitchClassToValue, pitchClassFromValue, pitchClassFromString
@docs accidentalToValue, accidentalFromValue, accidentalFromString
@docs diatonicPitchClassValue, diatonicPitchClassFromValue

-}

import String exposing (toLower)


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


{-| pitchClassToValue returns the chromatic position of a PitchClass relative to an octave
as a numeric value
-}
pitchClassToValue : PitchClass -> Int
pitchClassToValue class =
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


{-| pitchClassFromValue given a position relative to an octave returns the
corresponding key
-}
pitchClassFromValue : Int -> Maybe PitchClass
pitchClassFromValue value =
    case value of
        0 ->
            Just C

        2 ->
            Just D

        4 ->
            Just E

        5 ->
            Just F

        7 ->
            Just G

        9 ->
            Just A

        11 ->
            Just B

        _ ->
            Nothing


{-| pitchClassFromString parses a PitchClass from a String
-}
pitchClassFromString : String -> Maybe PitchClass
pitchClassFromString class =
    case toLower class of
        "c" ->
            Just C

        "d" ->
            Just D

        "e" ->
            Just E

        "f" ->
            Just F

        "g" ->
            Just G

        "a" ->
            Just A

        "b" ->
            Just B

        _ ->
            Nothing


pitchClassToString : PitchClass -> String
pitchClassToString k =
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


{-| diatonicPitchClassValue returns the diatonic position of a PitchClass relative to an octave
(composed of only natural notes (white notes of your piano)) as a numeric value.
-}
diatonicPitchClassValue : PitchClass -> Int
diatonicPitchClassValue class =
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


{-| diatonicPitchClassFromValue given a position relative to an octave
(composed of only natural notes (white notes of your piano)) returns the
corresponding key.
-}
diatonicPitchClassFromValue : Int -> Maybe PitchClass
diatonicPitchClassFromValue value =
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
