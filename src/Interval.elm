module Interval
    exposing
        ( Degree(..)
        , Interval(..)
        , distance
        , diatonicDegreeOf
        , intervalDegree
        , degreeToValue
        , substractDegree
        , intervalToValue
        , intervalFromValue
        , majorIntervals
        , minorIntervals
        , addInterval
        )

import Note exposing (Note, noteToIndex)
import Key
    exposing
        ( Tone
        , Adjustment(..)
        , newTone
        , adjustmentFromValue
        , diatonicKeyFromValue
        , diatonicKeyValue
        )


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
addInterval : Note -> Interval -> Note
addInterval note interval =
    let
        newNaturalNote =
            diatonicDegreeOf (intervalDegree interval) note

        intervalSemitones =
            intervalToValue interval

        startToNewNaturalSemitones =
            distance note newNaturalNote

        adjustment =
            adjustmentFromValue (intervalSemitones - startToNewNaturalSemitones)
    in
        { tone = newTone newNaturalNote.tone.key adjustment, octave = newNaturalNote.octave }


{-| diatonicDegreeOf will compute the note being the given
degree of a starting note on the diatonic scale
-}
diatonicDegreeOf : Degree -> Note -> Note
diatonicDegreeOf degree note =
    let
        diatonicKey =
            diatonicKeyFromValue <| (%) (diatonicKeyValue note.tone.key + degreeToValue degree) 7

        octaveShift =
            (//) (diatonicKeyValue note.tone.key + degreeToValue degree) 7
    in
        case diatonicKey of
            Just dk ->
                { tone = newTone dk Natural, octave = note.octave + octaveShift }

            Nothing ->
                note


{-| distance computes the distance in semitones between two notes
-}
distance : Note -> Note -> Int
distance from to =
    (-) (noteToIndex to) (noteToIndex from)


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
