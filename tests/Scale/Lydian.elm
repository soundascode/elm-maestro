module Scale.Lydian exposing (all)

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
        [ describe "Lydian scale tests"
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
        ]
