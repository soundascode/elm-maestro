module Degree exposing (Degree(..), intervalDegree, degreeToValue, substractDegree)

import Interval exposing (Interval(..))


{-|
-}
type Degree
    = First
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Octave
    | Ninth
    | Tenth
    | Eleventh
    | Twelfth
    | Thirteenth
    | Fourteenth


{-|
-}
intervalDegree : Interval -> Degree
intervalDegree interval =
    case interval of
        Unison ->
            First

        MinorSecond ->
            Second

        MajorSecond ->
            Second

        MinorThird ->
            Third

        MajorThird ->
            Third

        PerfectFourth ->
            Fourth

        PerfectFifth ->
            Fifth

        MinorSixth ->
            Sixth

        MajorSixth ->
            Sixth

        MinorSeventh ->
            Seventh

        MajorSeventh ->
            Seventh

        PerfectOctave ->
            Octave

        MinorNinth ->
            Ninth

        MajorNinth ->
            Ninth

        MinorTenth ->
            Tenth

        MajorTenth ->
            Tenth

        PerfectEleventh ->
            Eleventh

        AugmentedEleventh ->
            Eleventh

        PerfectTwelfth ->
            Twelfth

        MinorThirteen ->
            Thirteenth

        MajorThirteen ->
            Thirteenth

        MinorFourteenth ->
            Fourteenth

        MajorFourteenth ->
            Fourteenth

        DoubleOctave ->
            Octave


{-|
-}
degreeToValue : Degree -> Int
degreeToValue d =
    case d of
        First ->
            0

        Second ->
            1

        Third ->
            2

        Fourth ->
            3

        Fifth ->
            4

        Sixth ->
            5

        Seventh ->
            6

        Octave ->
            7

        Ninth ->
            8

        Tenth ->
            9

        Eleventh ->
            10

        Twelfth ->
            11

        Thirteenth ->
            12

        Fourteenth ->
            13


degreeFromValue : Int -> Maybe Degree
degreeFromValue value =
    case value of
        0 ->
            Just First

        1 ->
            Just Second

        2 ->
            Just Third

        3 ->
            Just Fourth

        4 ->
            Just Fifth

        5 ->
            Just Sixth

        6 ->
            Just Seventh

        7 ->
            Just Octave

        8 ->
            Just Ninth

        9 ->
            Just Tenth

        10 ->
            Just Eleventh

        11 ->
            Just Twelfth

        12 ->
            Just Thirteenth

        13 ->
            Just Fourteenth

        _ ->
            Nothing


substractDegree : Degree -> Int -> Degree
substractDegree degree value =
    let
        newDegree =
            degreeFromValue <| (degreeToValue degree) - value
    in
        case newDegree of
            Just nd ->
                nd

            Nothing ->
                degree
