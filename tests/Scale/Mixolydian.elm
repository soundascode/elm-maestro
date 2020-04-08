module Scale.Mixolydian exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass exposing (PitchClass(..))
import Maestro.Scale exposing (Scale(..), pitches)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Mixolydian scale tests"
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
        ]
