module Maestro.Interval exposing
    ( Interval(..)
    , addInterval
    , distance
    , diatonicDegreeOf
    )

{-| This module provides types and functions to compute, represent and
manipulate intervals.


# Types

@docs Degree, Interval


# Interval calculation

@docs addInterval, distance, diatonicDegreeOf


# Scales intervals

@docs ionianIntervals, dorianIntervals, phrygianIntervals, lydianIntervals
@docs mixolydianIntervals, aeolianIntervals, locrianIntervals

-}

import Maestro.Accidental exposing (Accidental(..), fromSemitones, toSemitones)
import Maestro.Note exposing (Note, noteToIndex)
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass
    exposing
        ( diatonicPitchClassFromValue
        , diatonicPitchClassValue
        )
import Maestro.Degree exposing (Degree(..))




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
    | AugmentedFourth
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


{-| addInterval applies an interval to a given note, and returns
the resulting note
-}
addInterval : Note -> Interval -> Note
addInterval note interval =
    let
        newDiatonicNote =
            diatonicDegreeOf (toDegree interval) note

        intervalSemitones =
            toSemitones interval

        startToNewDiatonicSemitones =
            distance note newDiatonicNote

        accidental =
            Maestro.Accidental.fromSemitones (intervalSemitones - startToNewDiatonicSemitones)

        newOctave =
            (noteToIndex newDiatonicNote + Maestro.Accidental.toSemitones accidental) // 12
    in
    { pitch = newPitch newDiatonicNote.pitch.class accidental, octave = newOctave }


{-| diatonicDegreeOf will compute the note being the given
degree of a starting note on the diatonic scale
-}
diatonicDegreeOf : Degree -> Note -> Note
diatonicDegreeOf degree note =
    let
        diatonicPitchClass =
            diatonicPitchClassFromValue <| remainderBy 7 (diatonicPitchClassValue note.pitch.class + Maestro.Degree.toValue degree)

        octaveShift =
            (diatonicPitchClassValue note.pitch.class + Maestro.Degree.toValue degree) // 7
    in
    case diatonicPitchClass of
        Just dk ->
            { pitch = newPitch dk Natural, octave = note.octave + octaveShift }

        Nothing ->
            note


{-| distance computes the distance in semitones between two notes
-}
distance : Note -> Note -> Int
distance from to =
    noteToIndex to - noteToIndex from


{-| intervalToSemitones returns the number of semitones corresponding
to the provided interval
-}
toSemitones : Interval -> Int
toSemitones interval =
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


{-| toDegree returns the degree of an interval. You could consider the
degree as the absolute value of an interval; an interval stripped of its modal
quality (Perfect, Major, minor, augmented, diminished).
-}
toDegree : Interval -> Degree
toDegree interval =
    case interval of
        PerfectUnison ->
            Root

        DiminishedSecond ->
            Second

        MinorSecond ->
            Second

        AugmentedUnison ->
            Root

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
