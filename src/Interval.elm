module Interval exposing (Interval(..), intervalToValue, intervalFromValue, majorIntervals, minorIntervals)

{-|
-}
type Interval
    = Unison
    | MinorSecond
    | MajorSecond
    | MinorThird
    | MajorThird
    | PerfectFourth
    | PerfectFifth
    | MinorSixth
    | MajorSixth
    | MinorSeventh
    | MajorSeventh
    | PerfectOctave
    | MinorNinth
    | MajorNinth
    | MinorTenth
    | MajorTenth
    | PerfectEleventh
    | AugmentedEleventh
    | PerfectTwelfth
    | MinorThirteen
    | MajorThirteen
    | MinorFourteenth
    | MajorFourteenth
    | DoubleOctave


{-|
-}
intervalToValue : Interval -> Int
intervalToValue interval =
    case interval of
        Unison ->
            0

        MinorSecond ->
            1

        MajorSecond ->
            2

        MinorThird ->
            3

        MajorThird ->
            4

        PerfectFourth ->
            5

        PerfectFifth ->
            7

        MinorSixth ->
            8

        MajorSixth ->
            9

        MinorSeventh ->
            10

        MajorSeventh ->
            11

        PerfectOctave ->
            12

        MinorNinth ->
            13

        MajorNinth ->
            14

        MinorTenth ->
            15

        MajorTenth ->
            16

        PerfectEleventh ->
            17

        AugmentedEleventh ->
            18

        PerfectTwelfth ->
            19

        MinorThirteen ->
            20

        MajorThirteen ->
            21

        MinorFourteenth ->
            22

        MajorFourteenth ->
            23

        DoubleOctave ->
            24


{-|
-}
intervalFromValue : Int -> Maybe Interval
intervalFromValue value =
    case value of
        0 ->
            Just Unison

        1 ->
            Just MinorSecond

        2 ->
            Just MajorSecond

        3 ->
            Just MinorThird

        4 ->
            Just MajorThird

        5 ->
            Just PerfectFourth

        7 ->
            Just PerfectFifth

        8 ->
            Just MinorSixth

        9 ->
            Just MajorSixth

        10 ->
            Just MinorSeventh

        11 ->
            Just MajorSeventh

        12 ->
            Just PerfectOctave

        13 ->
            Just MinorNinth

        14 ->
            Just MajorNinth

        15 ->
            Just MinorTenth

        16 ->
            Just MajorTenth

        17 ->
            Just PerfectEleventh

        18 ->
            Just AugmentedEleventh

        19 ->
            Just PerfectTwelfth

        20 ->
            Just MinorThirteen

        21 ->
            Just MajorThirteen

        22 ->
            Just MinorFourteenth

        23 ->
            Just MajorFourteenth

        24 ->
            Just DoubleOctave

        _ ->
            Nothing


{-|
-}
majorIntervals : List Interval
majorIntervals =
    [ Unison
    , MajorSecond
    , MajorThird
    , PerfectFourth
    , PerfectFifth
    , MajorSixth
    , MajorSeventh
    ]


{-|
-}
minorIntervals : List Interval
minorIntervals =
    [ Unison
    , MajorSecond
    , MinorThird
    , PerfectFourth
    , PerfectFifth
    , MinorSixth
    , MinorSeventh
    ]
