module Scale.Ionian exposing (all)

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
        [ describe "Ionian scale tests"
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
        ]
