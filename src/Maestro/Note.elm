module Maestro.Note exposing (..)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Types
@docs Octave, Note

# Common Helpers
@docs newNote, noteToIndex

-}

import Maestro.Pitch exposing (Pitch, Key, Adjustment, newPitch, adjustmentToValue, keyToValue)


{-|
-}
type alias Octave =
    Int


{-|
-}
type alias Note =
    { tone : Pitch
    , octave : Octave
    }


{-|
-}
newNote : Key -> Adjustment -> Octave -> Note
newNote key adjustment octave =
    { tone = newPitch key adjustment, octave = octave }


{-|
-}
noteToIndex : Note -> Int
noteToIndex note =
    note.octave * 12 + (keyToValue note.tone.key) + (adjustmentToValue note.tone.adjustment)
