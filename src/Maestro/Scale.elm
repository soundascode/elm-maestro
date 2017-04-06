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
import Maestro.Interval exposing (Interval(..), addInterval, majorIntervals, minorIntervals)


{-| Scale represents a list of tones composing it
-}
type alias Scale =
    List Tone


{-| Mode represents the mode of a scale.
-}
type Mode
    = Major
    | Minor


{-| Given a Tone and a Mode, generates the tones
composing a scale.

    (==)
      scale (newTone C Major) Major
      [ { key = C, accidental = Natural }
      , { key = D, accidental = Natural }
      , { key = E, accidental = Natural }
      , { key = F, accidental = Natural }
      , { key = G, accidental = Natural }
      , { key = A, accidental = Natural }
      , { key = B, accidental = Natural }
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
        Major ->
            majorIntervals

        Minor ->
            minorIntervals
