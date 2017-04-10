module Maestro.Interval
    exposing
        ( Degree(..)
        , Interval(..)
        , majorIntervals
        , minorIntervals
        , addInterval
        , distance
        , diatonicDegreeOf
        )

{-| This module provides types and functions to compute, represent and
manipulate intervals.

# Types
@docs Degree, Interval

# Common Helpers
@docs addInterval, majorIntervals, minorIntervals, distance, diatonicDegreeOf

-}

import Maestro.Note exposing (Note, noteToIndex)
import Maestro.Tone
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


{-| Interval represents the difference between two pitches
-}
type Interval
    = PerfectUnison
    | DiminishedSecond
    | MinorSecond
    | AugmentedUnison
    | MajorSecond
    | DiminishedThird
    | MinorThird
    | AugmentedSecond
    | MajorThird
    | DiminishedFourth
    | PerfectFourth
    | AugmentedThird
    | DiminishedFifth
      -- 6
    | AugmentedFourth
      -- 6
    | PerfectFifth
    | DiminishedSixth
    | MinorSixth
    | AugmentedFifth
    | MajorSixth
    | DiminishedSeventh
    | MinorSeventh
    | AugmentedSixth
    | MajorSeventh
    | DiminishedOctave
    | PerfectOctave
    | AugmentedSeventh
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


{-| intervalToValue returns the number of semitones corresponding
to the provided interval
-}
intervalToValue : Interval -> Int
intervalToValue interval =
    case interval of
        PerfectUnison ->
            0

        DiminishedSecond ->
            0

        MinorSecond ->
            1

        AugmentedUnison ->
            1

        MajorSecond ->
            2

        DiminishedThird ->
            2

        MinorThird ->
            3

        AugmentedSecond ->
            3

        MajorThird ->
            4

        DiminishedFourth ->
            4

        PerfectFourth ->
            5

        AugmentedThird ->
            5

        DiminishedFifth ->
            6

        AugmentedFourth ->
            6

        PerfectFifth ->
            7

        DiminishedSixth ->
            7

        MinorSixth ->
            8

        AugmentedFifth ->
            8

        MajorSixth ->
            9

        DiminishedSeventh ->
            9

        MinorSeventh ->
            10

        AugmentedSixth ->
            10

        MajorSeventh ->
            11

        DiminishedOctave ->
            11

        PerfectOctave ->
            12

        AugmentedSeventh ->
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


{-| addInterval applies an interval to a given note, and returns
the resulting note
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


{-| intervalFromValue computes the Interval from a number of semitones
-}
intervalFromValue : Int -> Maybe Interval
intervalFromValue value =
    case value of
        0 ->
            Just PerfectUnison

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

        _ ->
            Nothing


{-| majorIntervals represents the sequence of intervals composing
the Major scale
-}
majorIntervals : List Interval
majorIntervals =
    [ PerfectUnison
    , MajorSecond
    , MajorThird
    , PerfectFourth
    , PerfectFifth
    , MajorSixth
    , MajorSeventh
    ]


{-| minorIntervals represents the sequence of intervals composing
the minor scale
-}
minorIntervals : List Interval
minorIntervals =
    [ PerfectUnison
    , MajorSecond
    , MinorThird
    , PerfectFourth
    , PerfectFifth
    , MinorSixth
    , MinorSeventh
    ]


{-| intervalDegree returns the degree of an interval. You could consider the
degree as the absolute value of an interval; an interval stripped of its modal
quality (Perfect, Major, minor, augmented, diminished).
-}
intervalDegree : Interval -> Degree
intervalDegree interval =
    case interval of
        PerfectUnison ->
            First

        DiminishedSecond ->
            Second

        MinorSecond ->
            Second

        AugmentedUnison ->
            First

        MajorSecond ->
            Second

        DiminishedThird ->
            Third

        MinorThird ->
            Third

        AugmentedSecond ->
            Second

        MajorThird ->
            Third

        DiminishedFourth ->
            Fourth

        PerfectFourth ->
            Fourth

        AugmentedThird ->
            Third

        DiminishedFifth ->
            Fifth

        AugmentedFourth ->
            Fourth

        PerfectFifth ->
            Fifth

        DiminishedSixth ->
            Sixth

        MinorSixth ->
            Sixth

        AugmentedFifth ->
            Fifth

        MajorSixth ->
            Sixth

        DiminishedSeventh ->
            Seventh

        MinorSeventh ->
            Seventh

        AugmentedSixth ->
            Sixth

        MajorSeventh ->
            Seventh

        DiminishedOctave ->
            Octave

        PerfectOctave ->
            Octave

        AugmentedSeventh ->
            Seventh

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


{-| degreeToValue returns the numeric value of a degree
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


{-| degreeFromValue computes the degree represented by the provided value
-}
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


{-| substractDegree substracts value from a degree and returns the resulting
degree
-}
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
