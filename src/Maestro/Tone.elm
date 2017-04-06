module Maestro.Tone
    exposing
        ( Tone
        , Key(..)
        , Adjustment(..)
        , newTone
        , keyToValue
        , keyFromValue
        , diatonicKeyValue
        , diatonicKeyFromValue
        , adjustmentFromValue
        , adjustmentToValue
        )

{-| This module provides types and functions to manipulate musical tones.
It allows you to represent tones (pitches) like `C`, `C Sharp` and so on, as
well as helpers to represent these as numerical values.

# Types
@docs Tone, Key, Adjustment

# Common Helpers
@docs newTone, keyToValue, keyFromValue, diatonicKeyValue, diatonicKeyFromValue,
      adjustmentFromValue, adjustmentToValue

-}


{-| Key represents a Pitch class without adjustment
-}
type Key
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


{-| Tone represents a pitch and is defined by a key and an adjustment
-}
type alias Tone =
    { key : Key, adjustment : Adjustment }


{-| newTone is a helper function to create a tone
-}
newTone : Key -> Adjustment -> Tone
newTone key adjustment =
    { key = key, adjustment = adjustment }


{-| keyToValue returns the chromatic position of a Key relative to an octave
as a numeric value
-}
keyToValue : Key -> Int
keyToValue key =
    case key of
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
keyFromValue : Int -> Maybe Key
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


{-| diatonicKeyValue returns the diatonic position of a Key relative to an octave
(composed of only natural notes (white notes of your piano)) as a numeric value.
-}
diatonicKeyValue : Key -> Int
diatonicKeyValue key =
    case key of
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


{-| diatonicKeyFromValue given a position relative to an octave
(composed of only natural notes (white notes of your piano)) returns the
corresponding key.
-}
diatonicKeyFromValue : Int -> Maybe Key
diatonicKeyFromValue value =
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


{-| adjustmentToValue returns the numbers of semitones to apply to a
Key when calculating its position.
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
    case value of
        (-2) ->
            FlatFlat

        (-1) ->
            Flat

        0 ->
            Natural

        1 ->
            Sharp

        2 ->
            SharpSharp

        _ ->
            Natural
