module Maestro.Note exposing (..)

{-| This module provides types and functions to manipulate musical notes.
Notes being represented as a pitch and an octave convertible to an index (MIDI value)


# Types

@docs Octave, Note


# Common Helpers

@docs newNote, noteToIndex

-}

import Maestro.PitchClass exposing (Adjustment, Pitch, PitchClass, adjustmentToValue, chromaticPitches, newPitch, pitchClassToValue, pitchToIndex)


{-| Octave represents an octave number, as represented in piano or MIDI notation
-}
type alias Octave =
    Int


{-| Note represents a pitch on a given octave
-}
type alias Note =
    { pitch : Pitch
    , octave : Octave
    }


{-| newNote is a helper function to create a note
-}
newNote : PitchClass -> Adjustment -> Octave -> Note
newNote class adjustment oct =
    { pitch = newPitch class adjustment, octave = oct }


{-| noteToIndex returns the MIDI value of a given note
-}
noteToIndex : Note -> Int
noteToIndex note =
    note.octave * 12 + pitchToIndex note.pitch


octave : Int -> Adjustment -> List Note
octave number adj =
    List.map (\t -> newNote t.class t.adjustment number) (chromaticPitches adj)
