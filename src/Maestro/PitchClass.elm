module Maestro.PitchClass exposing
    ( Tone, PitchClass(..), Adjustment(..)
    , newTone, keyToValue, keyFromValue, keyFromString
    , adjustmentToValue, adjustmentFromValue, adjustmentFromString
    , diatonicPitchClassValue, diatonicPitchClassFromValue
    , adjustmentToString, chromaticTones, keyToString, toneToIndex, toneToString
    )

{-| This module provides types and functions to manipulate musical tones.
It allows you to represent tones (pitches) like `C`, `C Sharp` and so on, as
well as helpers to represent these as numerical values.


# Types

@docs Tone, PitchClass, Adjustment


# Common Helpers

@docs newTone, keyToValue, keyFromValue, keyFromString
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


{-| Tone represents a pitch and is defined by a class and an adjustment
-}
type alias Tone =
    { class : PitchClass, adjustment : Adjustment }


{-| newTone is a helper function to create a tone
-}
newTone : PitchClass -> Adjustment -> Tone
newTone class adjustment =
    { class = class, adjustment = adjustment }


{-| toneToIndex returns the index in an octave of the provided note. C would be zero,
while E Flat would be 3, or G Sharp would be 8.
-}
toneToIndex : Tone -> Int
toneToIndex t =
    remainderBy 12 (keyToValue t.class + adjustmentToValue t.adjustment)


toneToString : Tone -> String
toneToString t =
    keyToString t.class ++ adjustmentToString t.adjustment


{-| chromaticTones returns the chromatic scale tones starting at C.
The adjusted tones will be sharped or flatted according to the provided Adjustment.
This is mostly a helper function.
-}
chromaticTones : Adjustment -> List Tone
chromaticTones adj =
    let
        sharpedTones =
            [ newTone C Natural
            , newTone C Sharp
            , newTone D Natural
            , newTone D Sharp
            , newTone E Natural
            , newTone F Natural
            , newTone F Sharp
            , newTone G Natural
            , newTone G Sharp
            , newTone A Natural
            , newTone A Sharp
            , newTone B Natural
            ]

        flattedTones =
            [ newTone C Natural
            , newTone D Flat
            , newTone D Natural
            , newTone E Flat
            , newTone E Natural
            , newTone F Natural
            , newTone G Flat
            , newTone G Natural
            , newTone A Flat
            , newTone A Natural
            , newTone B Flat
            , newTone B Natural
            ]
    in
    case adj of
        Natural ->
            sharpedTones

        Sharp ->
            sharpedTones

        Flat ->
            flattedTones

        SharpSharp ->
            sharpedTones

        FlatFlat ->
            flattedTones


{-| keyToValue returns the chromatic position of a PitchClass relative to an octave
as a numeric value
-}
keyToValue : PitchClass -> Int
keyToValue class =
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


{-| keyFromValue given a position relative to an octave returns the
corresponding key
-}
keyFromValue : Int -> Maybe PitchClass
keyFromValue value =
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


{-| keyFromString parses a PitchClass from a String
-}
keyFromString : String -> Maybe PitchClass
keyFromString class =
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


keyToString : PitchClass -> String
keyToString k =
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


{-| adjustmentToValue returns the numbers of semitones to apply to a
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
number of semitones
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