module Maestro.Pitch
    exposing
        ( Pitch
        , Key(..)
        , Accidental(..)
        , newPitch
        , keyToValue
        , keyFromValue
        , diatonicKeyValue
        , diatonicKeyFromValue
        , adjustmentFromValue
        , adjustmentToValue
        )

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Types
@docs Pitch, Key, Accidental

# Common Helpers
@docs newPitch, keyToValue, keyFromValue, diatonicKeyValue, diatonicKeyFromValue,
      adjustmentFromValue, adjustmentToValue

-}


{-|
-}
type Key
    = C
    | D
    | E
    | F
    | G
    | A
    | B


{-|
-}
type Accidental
    = Natural
    | Sharp
    | Flat
    | SharpSharp
    | FlatFlat


{-|
-}
type alias Pitch =
    { key : Key, adjustment : Accidental }


{-|
-}
newPitch : Key -> Accidental -> Pitch
newPitch key adjustment =
    { key = key, adjustment = adjustment }


{-|
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


{-|
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


{-|
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


{-|
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


{-|
-}
adjustmentToValue : Accidental -> Int
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


{-|
-}
adjustmentFromValue : Int -> Accidental
adjustmentFromValue value =
    case value of
        (-1) ->
            Flat

        0 ->
            Natural

        1 ->
            Sharp

        (-2) ->
            FlatFlat

        2 ->
            SharpSharp

        _ ->
            Natural
