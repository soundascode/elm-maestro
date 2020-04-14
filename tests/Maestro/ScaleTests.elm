module Maestro.ScaleTests exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Interval exposing (Interval(..))
import Maestro.Degree exposing (Degree(..))
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass exposing (PitchClass(..))
import Maestro.Scale exposing (Scale(..), pitches)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Ionian (major) scale tests"
            [ test "C Ionian" <|
                \() ->
                    pitches (newPitch C Natural) Ionian
                    |> Expect.equal
                        [ newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        ]
            , test "C# Ionian" <|
                \() ->
                    pitches (newPitch C Sharp) Ionian
                    |> Expect.equal
                        [ newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        ]
            , test "Db Ionian" <|
                \() ->
                    pitches (newPitch D Flat) Ionian
                    |> Expect.equal
                        [ newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        ]
            , test "D Ionian" <|
                \() ->
                    pitches (newPitch D Natural) Ionian
                    |> Expect.equal
                        [ newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        ]
            , test "D# Ionian" <|
                \() ->
                    pitches (newPitch D Sharp) Ionian
                    |> Expect.equal
                        [ newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        ]
            , test "Eb Ionian" <|
                \() ->
                    pitches (newPitch E Flat) Ionian
                    |> Expect.equal
                        [ newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        ]
            , test "E Ionian" <|
                \() ->
                    pitches (newPitch E Natural) Ionian
                    |> Expect.equal
                        [ newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        ]
            , test "E# Ionian" <|
                \() ->
                    pitches (newPitch E Sharp) Ionian
                    |> Expect.equal
                        [ newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G SharpSharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        , newPitch D SharpSharp
                        ]
            , test "Fb Ionian" <|
                \() ->
                    pitches (newPitch F Flat) Ionian
                    |> Expect.equal
                        [ newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        ]
            , test "F Ionian" <|
                \() ->
                    pitches (newPitch F Natural) Ionian
                    |> Expect.equal
                        [ newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        ]
            , test "F# Ionian" <|
                \() ->
                    pitches (newPitch F Sharp) Ionian
                    |> Expect.equal
                        [ newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        ]
            , test "Gb Ionian" <|
                \() ->
                    pitches (newPitch G Flat) Ionian
                    |> Expect.equal
                        [ newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        ]
            , test "G Ionian" <|
                \() ->
                    pitches (newPitch G Natural) Ionian
                    |> Expect.equal
                        [ newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        ]
            , test "G# Ionian" <|
                \() ->
                    pitches (newPitch G Sharp) Ionian
                    |> Expect.equal
                        [ newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        ]
            , test "Ab Ionian" <|
                \() ->
                    pitches (newPitch A Flat) Ionian
                    |> Expect.equal
                        [ newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        ]
            , test "A Ionian" <|
                \() ->
                    pitches (newPitch A Natural) Ionian
                    |> Expect.equal
                        [ newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        ]
            , test "A# Ionian" <|
                \() ->
                    pitches (newPitch A Sharp) Ionian
                    |> Expect.equal
                        [ newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G SharpSharp
                        ]
            , test "Bb Ionian" <|
                \() ->
                    pitches (newPitch B Flat) Ionian
                    |> Expect.equal
                        [ newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        ]
            , test "B Ionian" <|
                \() ->
                    pitches (newPitch B Natural) Ionian
                    |> Expect.equal
                        [ newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        ]
            ]
        , describe "dorian mode tests"
            [ test "C Dorian" <|
                \() ->
                    pitches (newPitch C Natural) Dorian
                    |> Expect.equal
                        [ newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        ]
            , test "C# Dorian" <|
                \() ->
                    pitches (newPitch C Sharp) Dorian
                    |> Expect.equal
                        [ newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        ]
            , test "Db Dorian" <|
                \() ->
                    pitches (newPitch D Flat) Dorian
                    |> Expect.equal
                        [ newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Flat
                        ]
            , test "D Dorian" <|
                \() ->
                    pitches (newPitch D Natural) Dorian
                    |> Expect.equal
                        [ newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        ]
            , test "D# Dorian" <|
                \() ->
                    pitches (newPitch D Sharp) Dorian
                    |> Expect.equal
                        [ newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        ]
            , test "Eb Dorian" <|
                \() ->
                    pitches (newPitch E Flat) Dorian
                    |> Expect.equal
                        [ newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        ]
            , test "E Dorian" <|
                \() ->
                    pitches (newPitch E Natural) Dorian
                    |> Expect.equal
                        [ newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        ]
            , test "E# Dorian" <|
                \() ->
                    pitches (newPitch E Sharp) Dorian
                    |> Expect.equal
                        [ newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        , newPitch D Sharp
                        ]
            , test "Fb Dorian" <|
                \() ->
                    pitches (newPitch F Flat) Dorian
                    |> Expect.equal
                        [ newPitch F Flat
                        , newPitch G Flat
                        , newPitch A FlatFlat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E FlatFlat
                        ]
            , test "F Dorian" <|
                \() ->
                    pitches (newPitch F Natural) Dorian
                    |> Expect.equal
                        [ newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        ]
            , test "F# Dorian" <|
                \() ->
                    pitches (newPitch F Sharp) Dorian
                    |> Expect.equal
                        [ newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        ]
            , test "Gb Dorian" <|
                \() ->
                    pitches (newPitch G Flat) Dorian
                    |> Expect.equal
                        [ newPitch G Flat
                        , newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Flat
                        ]
            , test "G Dorian" <|
                \() ->
                    pitches (newPitch G Natural) Dorian
                    |> Expect.equal
                        [ newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        ]
            , test "G# Dorian" <|
                \() ->
                    pitches (newPitch G Sharp) Dorian
                    |> Expect.equal
                        [ newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        ]
            , test "Ab Dorian" <|
                \() ->
                    pitches (newPitch A Flat) Dorian
                    |> Expect.equal
                        [ newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Flat
                        ]
            , test "A Dorian" <|
                \() ->
                    pitches (newPitch A Natural) Dorian
                    |> Expect.equal
                        [ newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        ]
            , test "A# Dorian" <|
                \() ->
                    pitches (newPitch A Sharp) Dorian
                    |> Expect.equal
                        [ newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G Sharp
                        ]
            , test "Bb Dorian" <|
                \() ->
                    pitches (newPitch B Flat) Dorian
                    |> Expect.equal
                        [ newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        ]
            , test "B Dorian" <|
                \() ->
                    pitches (newPitch B Natural) Dorian
                    |> Expect.equal
                        [ newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        ]
            ]
        , describe "phrygian mode tests"
            [ test "C Phrygian" <|
                \() ->
                    pitches (newPitch C Natural) Phrygian
                    |> Expect.equal
                        [ newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        ]
            , test "C# Phrygian" <|
                \() ->
                    pitches (newPitch C Sharp) Phrygian
                    |> Expect.equal
                        [ newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        ]
            , test "Db Phrygian" <|
                \() ->
                    pitches (newPitch D Flat) Phrygian
                    |> Expect.equal
                        [ newPitch D Flat
                        , newPitch E FlatFlat
                        , newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        ]
            , test "D Phrygian" <|
                \() ->
                    pitches (newPitch D Natural) Phrygian
                    |> Expect.equal
                        [ newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        ]
            , test "D# Phrygian" <|
                \() ->
                    pitches (newPitch D Sharp) Phrygian
                    |> Expect.equal
                        [ newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        ]
            , test "Eb Phrygian" <|
                \() ->
                    pitches (newPitch E Flat) Phrygian
                    |> Expect.equal
                        [ newPitch E Flat
                        , newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        ]
            , test "E Phrygian" <|
                \() ->
                    pitches (newPitch E Natural) Phrygian
                    |> Expect.equal
                        [ newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        ]
            , test "E# Phrygian" <|
                \() ->
                    pitches (newPitch E Sharp) Phrygian
                    |> Expect.equal
                        [ newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        , newPitch D Sharp
                        ]
            , test "Fb Phrygian" <|
                \() ->
                    pitches (newPitch F Flat) Phrygian
                    |> Expect.equal
                        [ newPitch F Flat
                        , newPitch G FlatFlat
                        , newPitch A FlatFlat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D FlatFlat
                        , newPitch E FlatFlat
                        ]
            , test "F Phrygian" <|
                \() ->
                    pitches (newPitch F Natural) Phrygian
                    |> Expect.equal
                        [ newPitch F Natural
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        ]
            , test "F# Phrygian" <|
                \() ->
                    pitches (newPitch F Sharp) Phrygian
                    |> Expect.equal
                        [ newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        ]
            , test "Gb Phrygian" <|
                \() ->
                    pitches (newPitch G Flat) Phrygian
                    |> Expect.equal
                        [ newPitch G Flat
                        , newPitch A FlatFlat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E FlatFlat
                        , newPitch F Flat
                        ]
            , test "G Phrygian" <|
                \() ->
                    pitches (newPitch G Natural) Phrygian
                    |> Expect.equal
                        [ newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        ]
            , test "G# Phrygian" <|
                \() ->
                    pitches (newPitch G Sharp) Phrygian
                    |> Expect.equal
                        [ newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        ]
            , test "Ab Phrygian" <|
                \() ->
                    pitches (newPitch A Flat) Phrygian
                    |> Expect.equal
                        [ newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Flat
                        , newPitch G Flat
                        ]
            , test "A Phrygian" <|
                \() ->
                    pitches (newPitch A Natural) Phrygian
                    |> Expect.equal
                        [ newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        ]
            , test "A# Phrygian" <|
                \() ->
                    pitches (newPitch A Sharp) Phrygian
                    |> Expect.equal
                        [ newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        ]
            , test "Bb Phrygian" <|
                \() ->
                    pitches (newPitch B Flat) Phrygian
                    |> Expect.equal
                        [ newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Flat
                        , newPitch A Flat
                        ]
            , test "B Phrygian" <|
                \() ->
                    pitches (newPitch B Natural) Phrygian
                    |> Expect.equal
                        [ newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        ]
            ]
        , describe "Lydian scale tests"
            [ test "C Lydian" <|
                \() ->
                    pitches (newPitch C Natural) Lydian
                    |> Expect.equal
                        [ newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        ]
            , test "C# Lydian" <|
                \() ->
                    pitches (newPitch C Sharp) Lydian
                    |> Expect.equal
                        [ newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        ]
            , test "Db Lydian" <|
                \() ->
                    pitches (newPitch D Flat) Lydian
                    |> Expect.equal
                        [ newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        ]
            , test "D Lydian" <|
                \() ->
                    pitches (newPitch D Natural) Lydian
                    |> Expect.equal
                        [ newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        ]
            , test "D# Lydian" <|
                \() ->
                    pitches (newPitch D Sharp) Lydian
                    |> Expect.equal
                        [ newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G SharpSharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        ]
            , test "Eb Lydian" <|
                \() ->
                    pitches (newPitch E Flat) Lydian
                    |> Expect.equal
                        [ newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        ]
            , test "E Lydian" <|
                \() ->
                    pitches (newPitch E Natural) Lydian
                    |> Expect.equal
                        [ newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        ]
            , test "E# Lydian" <|
                \() ->
                    pitches (newPitch E Sharp) Lydian
                    |> Expect.equal
                        [ newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G SharpSharp
                        , newPitch A SharpSharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        , newPitch D SharpSharp
                        ]
            , test "Fb Lydian" <|
                \() ->
                    pitches (newPitch F Flat) Lydian
                    |> Expect.equal
                        [ newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        ]
            , test "F Lydian" <|
                \() ->
                    pitches (newPitch F Natural) Lydian
                    |> Expect.equal
                        [ newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        ]
            , test "F# Lydian" <|
                \() ->
                    pitches (newPitch F Sharp) Lydian
                    |> Expect.equal
                        [ newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        ]
            , test "Gb Lydian" <|
                \() ->
                    pitches (newPitch G Flat) Lydian
                    |> Expect.equal
                        [ newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        ]
            , test "G Lydian" <|
                \() ->
                    pitches (newPitch G Natural) Lydian
                    |> Expect.equal
                        [ newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        ]
            , test "G# Lydian" <|
                \() ->
                    pitches (newPitch G Sharp) Lydian
                    |> Expect.equal
                        [ newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        ]
            , test "Ab Lydian" <|
                \() ->
                    pitches (newPitch A Flat) Lydian
                    |> Expect.equal
                        [ newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        ]
            , test "A Lydian" <|
                \() ->
                    pitches (newPitch A Natural) Lydian
                    |> Expect.equal
                        [ newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        ]
            , test "A# Lydian" <|
                \() ->
                    pitches (newPitch A Sharp) Lydian
                    |> Expect.equal
                        [ newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        , newPitch D SharpSharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G SharpSharp
                        ]
            , test "Bb Lydian" <|
                \() ->
                    pitches (newPitch B Flat) Lydian
                    |> Expect.equal
                        [ newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        ]
            , test "B Lydian" <|
                \() ->
                    pitches (newPitch B Natural) Lydian
                    |> Expect.equal
                        [ newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        ]
            ]
        , describe "Mixolydian scale tests"
            [ test "C Mixolydian" <|
                \() ->
                    pitches (newPitch C Natural) Mixolydian
                    |> Expect.equal
                        [ newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        ]
            , test "C# Mixolydian" <|
                \() ->
                    pitches (newPitch C Sharp) Mixolydian
                    |> Expect.equal
                        [ newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        ]
            , test "Db Mixolydian" <|
                \() ->
                    pitches (newPitch D Flat) Mixolydian
                    |> Expect.equal
                        [ newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Flat
                        ]
            , test "D Mixolydian" <|
                \() ->
                    pitches (newPitch D Natural) Mixolydian
                    |> Expect.equal
                        [ newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        ]
            , test "D# Mixolydian" <|
                \() ->
                    pitches (newPitch D Sharp) Mixolydian
                    |> Expect.equal
                        [ newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        ]
            , test "Eb Mixolydian" <|
                \() ->
                    pitches (newPitch E Flat) Mixolydian
                    |> Expect.equal
                        [ newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        ]
            , test "E Mixolydian" <|
                \() ->
                    pitches (newPitch E Natural) Mixolydian
                    |> Expect.equal
                        [ newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        ]
            , test "E# Mixolydian" <|
                \() ->
                    pitches (newPitch E Sharp) Mixolydian
                    |> Expect.equal
                        [ newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G SharpSharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        , newPitch D Sharp
                        ]
            , test "Fb Mixolydian" <|
                \() ->
                    pitches (newPitch F Flat) Mixolydian
                    |> Expect.equal
                        [ newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E FlatFlat
                        ]
            , test "F Mixolydian" <|
                \() ->
                    pitches (newPitch F Natural) Mixolydian
                    |> Expect.equal
                        [ newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        ]
            , test "F# Mixolydian" <|
                \() ->
                    pitches (newPitch F Sharp) Mixolydian
                    |> Expect.equal
                        [ newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        ]
            , test "Gb Mixolydian" <|
                \() ->
                    pitches (newPitch G Flat) Mixolydian
                    |> Expect.equal
                        [ newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Flat
                        ]
            , test "G Mixolydian" <|
                \() ->
                    pitches (newPitch G Natural) Mixolydian
                    |> Expect.equal
                        [ newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        ]
            , test "G# Mixolydian" <|
                \() ->
                    pitches (newPitch G Sharp) Mixolydian
                    |> Expect.equal
                        [ newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        ]
            , test "Ab Mixolydian" <|
                \() ->
                    pitches (newPitch A Flat) Mixolydian
                    |> Expect.equal
                        [ newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Flat
                        ]
            , test "A Mixolydian" <|
                \() ->
                    pitches (newPitch A Natural) Mixolydian
                    |> Expect.equal
                        [ newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        ]
            , test "A# Mixolydian" <|
                \() ->
                    pitches (newPitch A Sharp) Mixolydian
                    |> Expect.equal
                        [ newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G Sharp
                        ]
            , test "Bb Mixolydian" <|
                \() ->
                    pitches (newPitch B Flat) Mixolydian
                    |> Expect.equal
                        [ newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        ]
            , test "B Mixolydian" <|
                \() ->
                    pitches (newPitch B Natural) Mixolydian
                    |> Expect.equal
                        [ newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        ]
            ]
        , describe "Aeolian (minor) scale tests"
            [ test "C Aeolian" <|
                \() ->
                    pitches (newPitch C Natural) Aeolian
                    |> Expect.equal
                        [ newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        ]
            , test "C# Aeolian" <|
                \() ->
                    pitches (newPitch C Sharp) Aeolian
                    |> Expect.equal
                        [ newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        ]
            , test "D Aeolian" <|
                \() ->
                    pitches (newPitch D Natural) Aeolian
                    |> Expect.equal
                        [ newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        ]
            , test "D# Aeolian" <|
                \() ->
                    pitches (newPitch D Sharp) Aeolian
                    |> Expect.equal
                        [ newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        ]
            , test "E Aeolian" <|
                \() ->
                    pitches (newPitch E Natural) Aeolian
                    |> Expect.equal
                        [ newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        ]
            , test "E# Aeolian" <|
                \() ->
                    pitches (newPitch E Sharp) Aeolian
                    |> Expect.equal
                        [ newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        , newPitch D Sharp
                        ]
            , test "F Aeolian" <|
                \() ->
                    pitches (newPitch F Natural) Aeolian
                    |> Expect.equal
                        [ newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        ]
            , test "F# Aeolian" <|
                \() ->
                    pitches (newPitch F Sharp) Aeolian
                    |> Expect.equal
                        [ newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        ]
            , test "G Aeolian" <|
                \() ->
                    pitches (newPitch G Natural) Aeolian
                    |> Expect.equal
                        [ newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        ]
            , test "G# Aeolian" <|
                \() ->
                    pitches (newPitch G Sharp) Aeolian
                    |> Expect.equal
                        [ newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        ]
            , test "A Aeolian" <|
                \() ->
                    pitches (newPitch A Natural) Aeolian
                    |> Expect.equal
                        [ newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        ]
            , test "A# Aeolian" <|
                \() ->
                    pitches (newPitch A Sharp) Aeolian
                    |> Expect.equal
                        [ newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        ]
            , test "B Aeolian" <|
                \() ->
                    pitches (newPitch B Natural) Aeolian
                    |> Expect.equal
                        [ newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        ]
            ]
        , describe "locrian scale tests"
            [ test "C locrian" <|
                \() ->
                    pitches (newPitch C Natural) Locrian
                    |> Expect.equal
                        [ newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        ]
            , test "C# locrian" <|
                \() ->
                    pitches (newPitch C Sharp) Locrian
                    |> Expect.equal
                        [ newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        ]
            , test "Db locrian" <|
                \() ->
                    pitches (newPitch D Flat) Locrian
                    |> Expect.equal
                        [ newPitch D Flat
                        , newPitch E FlatFlat
                        , newPitch F Flat
                        , newPitch G Flat
                        , newPitch A FlatFlat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        ]
            , test "D locrian" <|
                \() ->
                    pitches (newPitch D Natural) Locrian
                    |> Expect.equal
                        [ newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        ]
            , test "D# locrian" <|
                \() ->
                    pitches (newPitch D Sharp) Locrian
                    |> Expect.equal
                        [ newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        ]
            , test "Eb locrian" <|
                \() ->
                    pitches (newPitch E Flat) Locrian
                    |> Expect.equal
                        [ newPitch E Flat
                        , newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        ]
            , test "E locrian" <|
                \() ->
                    pitches (newPitch E Natural) Locrian
                    |> Expect.equal
                        [ newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        ]
            , test "E# locrian" <|
                \() ->
                    pitches (newPitch E Sharp) Locrian
                    |> Expect.equal
                        [ newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        ]
            , test "Fb locrian" <|
                \() ->
                    pitches (newPitch F Flat) Locrian
                    |> Expect.equal
                        [ newPitch F Flat
                        , newPitch G FlatFlat
                        , newPitch A FlatFlat
                        , newPitch B FlatFlat
                        , newPitch C FlatFlat
                        , newPitch D FlatFlat
                        , newPitch E FlatFlat
                        ]
            , test "F locrian" <|
                \() ->
                    pitches (newPitch F Natural) Locrian
                    |> Expect.equal
                        [ newPitch F Natural
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        ]
            , test "F# locrian" <|
                \() ->
                    pitches (newPitch F Sharp) Locrian
                    |> Expect.equal
                        [ newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        ]
            , test "Gb locrian" <|
                \() ->
                    pitches (newPitch G Flat) Locrian
                    |> Expect.equal
                        [ newPitch G Flat
                        , newPitch A FlatFlat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D FlatFlat
                        , newPitch E FlatFlat
                        , newPitch F Flat
                        ]
            , test "G locrian" <|
                \() ->
                    pitches (newPitch G Natural) Locrian
                    |> Expect.equal
                        [ newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        ]
            , test "G# locrian" <|
                \() ->
                    pitches (newPitch G Sharp) Locrian
                    |> Expect.equal
                        [ newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        ]
            , test "Ab locrian" <|
                \() ->
                    pitches (newPitch A Flat) Locrian
                    |> Expect.equal
                        [ newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E FlatFlat
                        , newPitch F Flat
                        , newPitch G Flat
                        ]
            , test "A locrian" <|
                \() ->
                    pitches (newPitch A Natural) Locrian
                    |> Expect.equal
                        [ newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        ]
            , test "A# locrian" <|
                \() ->
                    pitches (newPitch A Sharp) Locrian
                    |> Expect.equal
                        [ newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        ]
            , test "Bb locrian" <|
                \() ->
                    pitches (newPitch B Flat) Locrian
                    |> Expect.equal
                        [ newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        ]
            , test "B locrian" <|
                \() ->
                    pitches (newPitch B Natural) Locrian
                    |> Expect.equal
                        [ newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        ]
            ]
        ]
