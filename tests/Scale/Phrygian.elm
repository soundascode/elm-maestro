module Scale.Phrygian exposing (all)

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
    describe "Phrygian test suite"
        [ describe "phrygian mode tests"
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
        ]
