module Maestro.Pitch exposing
    ( Pitch
    , chromaticPitches
    , newPitch
    , pitchToIndex
    , pitchToString
    )

import Maestro.Accidental
    exposing
        ( Accidental(..)
        , toString
        , toSemitones
        )
import Maestro.PitchClass
    exposing
        ( PitchClass(..)
        , pitchClassToString
        , pitchClassToValue
        )


{-| Pitch represents a pitch and is defined by a class and an accidental
-}
type alias Pitch =
    { class : PitchClass
    , accidental : Accidental
    }


{-| newPitch is a helper function to create a pitch
-}
newPitch : PitchClass -> Accidental -> Pitch
newPitch class accidental =
    { class = class, accidental = accidental }


{-| pitchToIndex returns the index in an octave of the provided note. C would be zero,
while E Flat would be 3, or G Sharp would be 8.
-}
pitchToIndex : Pitch -> Int
pitchToIndex t =
    remainderBy 12 (pitchClassToValue t.class + Maestro.Accidental.toSemitones t.accidental)


pitchToString : Pitch -> String
pitchToString t =
    pitchClassToString t.class ++ Maestro.Accidental.toString t.accidental


{-| chromaticPitches returns the chromatic scale pitches starting at C.
The adjusted pitches will be sharped or flatted according to the provided Accidental.
This is mostly a helper function.
-}
chromaticPitches : Accidental -> List Pitch
chromaticPitches adj =
    let
        sharpedPitches =
            [ newPitch C Natural
            , newPitch C Sharp
            , newPitch D Natural
            , newPitch D Sharp
            , newPitch E Natural
            , newPitch F Natural
            , newPitch F Sharp
            , newPitch G Natural
            , newPitch G Sharp
            , newPitch A Natural
            , newPitch A Sharp
            , newPitch B Natural
            ]

        flattedPitches =
            [ newPitch C Natural
            , newPitch D Flat
            , newPitch D Natural
            , newPitch E Flat
            , newPitch E Natural
            , newPitch F Natural
            , newPitch G Flat
            , newPitch G Natural
            , newPitch A Flat
            , newPitch A Natural
            , newPitch B Flat
            , newPitch B Natural
            ]
    in
    case adj of
        Natural ->
            sharpedPitches

        Sharp ->
            sharpedPitches

        Flat ->
            flattedPitches

        SharpSharp ->
            sharpedPitches

        FlatFlat ->
            flattedPitches
