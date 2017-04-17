module Maestro.Note exposing (..)

{-| This module provides types and functions to manipulate musical notes.
Notes being represented as a tone and an octave convertible to an index (MIDI value)

# Types
@docs Octave, Note

# Common Helpers
@docs newNote, noteToIndex

-}

import Maestro.Tone exposing (Tone, Key, Adjustment, newTone, toneToIndex, chromaticTones, adjustmentToValue, keyToValue)


{-| Octave represents an octave number, as represented in piano or MIDI notation
-}
type alias Octave =
    Int


{-| Note represents a tone on a given octave
-}
type alias Note =
    { tone : Tone
    , octave : Octave
    }


{-| newNote is a helper function to create a note
-}
newNote : Key -> Adjustment -> Octave -> Note
newNote key adjustment octave =
    { tone = newTone key adjustment, octave = octave }


{-| noteToIndex returns the MIDI value of a given note
-}
noteToIndex : Note -> Int
noteToIndex note =
    note.octave * 12 + (toneToIndex note.tone)


octave : Int -> Adjustment -> List Note
octave number adj =
    List.map (\t -> newNote t.key t.adjustment number) (chromaticTones adj)
