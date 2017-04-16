module Maestro.Scale exposing (Scale, Mode(..), scale)

{-| This module provides types and functions to create and
manipulate scales.

# Types
@docs Scale, Mode

# Scales manipulation
@docs scale

-}

import Maestro.Tone exposing (Tone)
import Maestro.Note exposing (Note, newNote)
import Maestro.Interval
    exposing
        ( Interval(..)
        , addInterval
        , ionianIntervals
        , dorianIntervals
        , aeolianIntervals
        )


{-| Scale represents a list of tones composing it
-}
type alias Scale =
    List Tone


{-| Mode represents the mode of a scale.
-}
type Mode
    = Ionian
    | Dorian
    | Aeolian


{-| Given a Tone and a Mode, generates the tones
composing a scale.

    (==)
      scale (newTone C Natural) Major
      [ { key = C, adjustment = Natural }
      , { key = D, adjustment = Natural }
      , { key = E, adjustment = Natural }
      , { key = F, adjustment = Natural }
      , { key = G, adjustment = Natural }
      , { key = A, adjustment = Natural }
      , { key = B, adjustment = Natural }
      ]
-}
scale : Tone -> Mode -> List Tone
scale tone mode =
    let
        placeholderNote =
            newNote tone.key tone.adjustment 3
    in
        List.map (\i -> (addInterval placeholderNote i).tone) (modeToIntervals mode)


modeToIntervals : Mode -> List Interval
modeToIntervals mode =
    case mode of
        Ionian ->
            ionianIntervals

        Dorian ->
            dorianIntervals

        Aeolian ->
            aeolianIntervals
