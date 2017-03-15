module MusicTheory exposing (Key(..), Mode(..), Adjustment(..), Note, Scale, Octave, scale, diatonicDegreeOf, distance)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs Key, Mode, Adjustment, Note, Scale, Octave

# Common Helpers
@docs scale, diatonicDegreeOf, distance

-}

import Interval exposing (..)
import Degree exposing (Degree(..), degreeToValue)

import List
import Tuple exposing (first, second)


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


{-|
-}
type alias Octave =
    Int


{-|
-}
type alias Scale =
    List Int


{-|
-}
type Mode
    = Major
    | Minor


{-|
-}
type alias Note =
    { key : Key
    , adjustment : Adjustment
    , octave : Octave
    }


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


{-| diatonicDegreeOf will compute the note being the given
degree of a starting note on the diatonic scale
-}
diatonicDegreeOf : Degree -> Key -> Maybe Key
diatonicDegreeOf degree key =
    diatonicKeyFromValue <| (%) (diatonicKeyValue key + degreeToValue degree) 7


{-| distance computes the distance in semitones between two notes
-}
distance : Note -> Note -> Int
distance from to =
    (-) (noteToIndex to) (noteToIndex from)


-- addInterval : Note -> Interval -> Note
-- addInterval note interval =
--     let
--       degree = intervalDegree interval
--         newNaturalNote
--     in


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
octave : Int -> List Int
octave n =
    List.range (0 + (n * 12)) (11 + (n * 12))


{-|
-}
octaveOf : Int -> Int
octaveOf value =
    value // 12


{-|
-}
noteOf : Int -> Int
noteOf value =
    value % 12


{-|
-}
keyAtOctave : Key -> Int -> Int
keyAtOctave key octave =
    (keyToValue key) + (12 * octave)


{-|
-}
octaveWhiteKeys : Octave -> List Int
octaveWhiteKeys o =
    List.map (\k -> keyAtOctave k o) [ C, D, E, F, G, A, B ]


{-|
-}
octaveBlackKeys : Octave -> List Int
octaveBlackKeys o =
    let
        whiteKeys =
            octaveWhiteKeys o
    in
        List.filter (\v -> notContains whiteKeys v) (octave o)


{-|
-}
adjustmentToValue : Adjustment -> Int
adjustmentToValue adjustment =
    case adjustment of
        Flat ->
            -1

        Natural ->
            0

        Sharp ->
            1


{-|
-}
adjustmentFromValue : Int -> Maybe Adjustment
adjustmentFromValue value =
    case value of
        -1 ->
            Just Flat

        0 ->
            Just Natural

        1 ->
            Just Sharp

        _ ->
            Nothing


{-|
-}
modeToIntervals : Mode -> List Interval
modeToIntervals mode =
    case mode of
        Major ->
            majorIntervals

        Minor ->
            minorIntervals


{-|
-}
noteToIndex : Note -> Int
noteToIndex note =
    note.octave * 12 + (keyToValue note.key) + (adjustmentToValue note.adjustment)


-- addInterval : Note -> Interval -> Note
-- addInterval note interval =
--     let
--         baseNoteIndex =
--             noteOf <| noteToIndex note
--
--         newNaturalNoteIndex =
--             baseNoteIndex + degreeToValue (intervalDegree interval)
--     in
--         { key = C, octave = 3, adjustment = Natural }


{-|
-}
scale : Note -> Mode -> Scale
scale note mode =
    List.map (\i -> (noteToIndex note) + intervalToValue i) (modeToIntervals mode)


{-|
-}
isWhite : Int -> Bool
isWhite value =
    let
        note =
            keyFromValue (value % 12)
    in
        case note of
            Just n ->
                True

            Nothing ->
                False


{-|
-}
isBlack : Int -> Bool
isBlack value =
    not <| isWhite value


{-|
-}
contains : List a -> a -> Bool
contains seq v =
    let
        head =
            List.filter (\e -> e == v) seq
    in
        (List.length head) /= 0


{-|
-}
notContains : List a -> a -> Bool
notContains seq v =
    let
        head =
            List.filter (\e -> e == v) seq
    in
        (List.length head) == 0
