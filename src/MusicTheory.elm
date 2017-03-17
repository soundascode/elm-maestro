module MusicTheory exposing (Note, newNote, diatonicDegreeOf, distance, addInterval)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs Note

# Common Helpers
@docs diatonicDegreeOf, distance, addInterval, newNote

-}

import Key
    exposing
        ( Tone
        , Key(..)
        , Adjustment(..)
        , newTone
        , keyToValue
        , keyFromValue
        , adjustmentToValue
        , adjustmentFromValue
        , diatonicKeyValue
        , diatonicKeyFromValue
        )
import Interval exposing (Interval(..), intervalToValue, minorIntervals, majorIntervals)
import Degree exposing (Degree(..), degreeToValue, intervalDegree, substractDegree)
import List
import Tuple exposing (first, second)


{-|
-}
type alias Octave =
    Int


{-|
-}
type alias Note =
    { tone : Tone
    , octave : Octave
    }


{-|
-}
newNote : Key -> Adjustment -> Octave -> Note
newNote key adjustment octave =
    { tone = newTone key adjustment, octave = octave }


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
noteToIndex : Note -> Int
noteToIndex note =
    note.octave * 12 + (keyToValue note.tone.key) + (adjustmentToValue note.tone.adjustment)
