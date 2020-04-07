module Maestro.PitchClass exposing
    ( Pitch, PitchClass(..), Adjustment(..)
    , newPitch, pitchClassToValue, pitchClassFromValue, pitchClassFromString
    , adjustmentToValue, adjustmentFromValue, adjustmentFromString
    , diatonicPitchClassValue, diatonicPitchClassFromValue
    , adjustmentToString, chromaticPitches, pitchClassToString, pitchToIndex, pitchToString
    )

{-| This module provides types and functions to manipulate musical pitches.
It allows you to represent pitches (pitches) like `C`, `C Sharp` and so on, as
well as helpers to represent these as numerical values.


# Types

@docs Pitch, PitchClass, Adjustment


# Common Helpers

@docs newPitch, pitchClassToValue, pitchClassFromValue, pitchClassFromString
@docs adjustmentToValue, adjustmentFromValue, adjustmentFromString
@docs diatonicPitchClassValue, diatonicPitchClassFromValue

-}

import String exposing (toLower)


{-| PitchClass represents a Pitch class without adjustment
-}
type PitchClass
    = C
    | D
    | E
    | F
    | G
    | A
    | B


{-| Adjustment represents an adjustment applied to a key
-}
type Adjustment
    = Natural
    | Sharp
    | Flat
    | SharpSharp
    | FlatFlat


{-| Pitch represents a pitch and is defined by a class and an adjustment
-}
type alias Pitch =
    { class : PitchClass, adjustment : Adjustment }


{-| newPitch is a helper function to create a pitch
-}
newPitch : PitchClass -> Adjustment -> Pitch
newPitch class adjustment =
    { class = class, adjustment = adjustment }


{-| pitchToIndex returns the index in an octave of the provided note. C would be zero,
while E Flat would be 3, or G Sharp would be 8.
-}
pitchToIndex : Pitch -> Int
pitchToIndex t =
    remainderBy 12 (pitchClassToValue t.class + adjustmentToValue t.adjustment)


pitchToString : Pitch -> String
pitchToString t =
    pitchClassToString t.class ++ adjustmentToString t.adjustment


{-| chromaticPitches returns the chromatic scale pitches starting at C.
The adjusted pitches will be sharped or flatted according to the provided Adjustment.
This is mostly a helper function.
-}
chromaticPitches : Adjustment -> List Pitch
chromaticPitches adj =
    let
        sharpedPitches =
            [ newPitch C Natural
            , newPitch C Sharp
            , newPitch D Natural
            , newPitch D Sharp
            , newPitch E Natural
            , newPitch F Natural
            , newPitch F Sharp
            , newPitch G Natural
            , newPitch G Sharp
            , newPitch A Natural
            , newPitch A Sharp
            , newPitch B Natural
            ]

        flattedPitches =
            [ newPitch C Natural
            , newPitch D Flat
            , newPitch D Natural
            , newPitch E Flat
            , newPitch E Natural
            , newPitch F Natural
            , newPitch G Flat
            , newPitch G Natural
            , newPitch A Flat
            , newPitch A Natural
            , newPitch B Flat
            , newPitch B Natural
            ]
    in
    case adj of
        Natural ->
            sharpedPitches

        Sharp ->
            sharpedPitches

        Flat ->
            flattedPitches

        SharpSharp ->
            sharpedPitches

        FlatFlat ->
            flattedPitches


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


{-| adjustmentToValue returns the numbers of semipitches to apply to a
PitchClass when calculating its position.
-}
adjustmentToValue : Adjustment -> Int
adjustmentToValue adjustment =
    case adjustment of
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


{-| adjustmentFromValue returns the adjustment corresponding to a given
number of semipitches
-}
adjustmentFromValue : Int -> Adjustment
adjustmentFromValue value =
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


{-| adjustmentFromString parses an adjustment from a String
-}
adjustmentFromString : String -> Maybe Adjustment
adjustmentFromString adj =
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


{-| adjustmentFromString parses an adjustment from a String
-}
adjustmentToString : Adjustment -> String
adjustmentToString adj =
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
