module Maestro.Pitch
    exposing
        ( Pitch
        , chromatics
        , newPitch
        , toSemitones
        , toString
        )

{-| This module provides abstractions and helpers to represent and manipulate pitches.
A pitch is defined by a PitchClass and an Accidental. It makes it possible to represent any pitches
of the western music system. It is the base building block of the Note type which adds the notion
of Octave to it.

# Definition
@docs Pitch

# Helpers
@docs newPitch, chromatics

# Conversion
@docs toSemitones, toString
-}

import Maestro.Accidental
    exposing
        ( Accidental(..)
        , toString
        , toSemitones
        )
import Maestro.PitchClass
    exposing
        ( PitchClass(..)
        , toString
        , toSemitones
        )


{-| Represents a pitch, as defined by a class and an accidental.
-}
type alias Pitch =
    { class : PitchClass
    , accidental :
        Accidental
        -- // TODO: Should accidental be an optional (Maybe)?
    }


{-| Helper function to create a pitch from a PitchClass and an Accidental

    newPitch C Sharp == { class = C, accidental = Sharp}
-}
newPitch : PitchClass -> Accidental -> Pitch
newPitch class accidental =
    { class = class, accidental = accidental }


{-| Translates a Pitch to the number of semitones it represents.
Like on a piano, it is indexed on C (0).

    toSemitones <| newPitch C Natural == 0
    toSemitones <| newPitch E Flat == 3
    toSemitones <| newPitch G Sharp == 8
-}
toSemitones : Pitch -> Int
toSemitones t =
    remainderBy 12 (Maestro.PitchClass.toSemitones t.class + Maestro.Accidental.toSemitones t.accidental)


{-| Converts a Pitch into its string represenation

    toString <| newPitch C Sharp == "C♯"
    toString <| newPitch G Flat == "G♭"
-}
toString : Pitch -> String
toString t =
    Maestro.PitchClass.toString t.class ++ Maestro.Accidental.toString t.accidental


{-| Produces the list of notes out of the chromatic scale, restricting accidentals to the provided one.
It is useful to obtain the list of all possible notes, while restricting the (piano) black notes to whether
sharps or flats.

    chromatics Sharp ==
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
    chromatics Flat ==
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
-}
chromatics : Accidental -> List Pitch
chromatics adj =
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
