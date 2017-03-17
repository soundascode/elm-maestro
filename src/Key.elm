module Key
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

{-|
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
type Adjustment
    = Natural
    | Sharp
    | Flat
    | SharpSharp
    | FlatFlat


type alias Tone =
    { key : Key, adjustment : Adjustment }


newTone : Key -> Adjustment -> Tone
newTone key adjustment =
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


{-|
-}
adjustmentFromValue : Int -> Adjustment
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
