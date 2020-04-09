module Scale.Aeolian exposing (all)

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
        [ describe "minor scale tests"
            [ test "C minor" <|
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
            , test "C# minor" <|
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
            , test "D minor" <|
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
            , test "D# minor" <|
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
            , test "E minor" <|
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
            , test "E# minor" <|
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
            , test "F minor" <|
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
            , test "F# minor" <|
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
            , test "G minor" <|
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
            , test "G# minor" <|
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
            , test "A minor" <|
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
            , test "A# minor" <|
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
            , test "B minor" <|
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
        ]
