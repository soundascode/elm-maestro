module Maestro exposing (..)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs octaveOf, noteOf, keyAtOctave
-}

import Maestro.Pitch exposing (Key, keyToValue)


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
