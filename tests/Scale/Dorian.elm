module Scale.Dorian exposing (all)

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
    describe "Dorian test suite"
        [ describe "dorian mode tests"
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
        ]
