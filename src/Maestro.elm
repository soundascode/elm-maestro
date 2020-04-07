module Maestro exposing (..)

{-| This library provides music theory abstractions and functionalities.


# Definition

@docs octaveOf, noteOf, keyAtOctave

-}

import Maestro.PitchClass exposing (PitchClass, pitchClassToValue)


{-| -}
octaveOf : Int -> Int
octaveOf value =
    value // 12


{-| -}
noteOf : Int -> Int
noteOf value =
    remainderBy 12 value


{-| -}
keyAtOctave : PitchClass -> Int -> Int
keyAtOctave class octave =
    pitchClassToValue class + (12 * octave)
