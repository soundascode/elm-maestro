module Degree exposing (Degree(..), intervalDegree, degreeToValue)

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
