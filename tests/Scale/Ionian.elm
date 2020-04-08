module Scale.Ionian exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass exposing (PitchClass(..))
import Maestro.Scale exposing (Mode(..), scale)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Ionian scale tests"
            [ test "C Ionian" <|
                \() ->
                    Expect.equal (scale (newPitch C Natural) Ionian)
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
                    Expect.equal (scale (newPitch C Sharp) Ionian)
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
                    Expect.equal (scale (newPitch D Flat) Ionian)
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
                    Expect.equal (scale (newPitch D Natural) Ionian)
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
                    Expect.equal (scale (newPitch D Sharp) Ionian)
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
                    Expect.equal (scale (newPitch E Flat) Ionian)
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
                    Expect.equal (scale (newPitch E Natural) Ionian)
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
                    Expect.equal (scale (newPitch E Sharp) Ionian)
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
                    Expect.equal (scale (newPitch F Flat) Ionian)
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
                    Expect.equal (scale (newPitch F Natural) Ionian)
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
                    Expect.equal (scale (newPitch F Sharp) Ionian)
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
                    Expect.equal (scale (newPitch G Flat) Ionian)
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
                    Expect.equal (scale (newPitch G Natural) Ionian)
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
                    Expect.equal (scale (newPitch G Sharp) Ionian)
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
                    Expect.equal (scale (newPitch A Flat) Ionian)
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
                    Expect.equal (scale (newPitch A Natural) Ionian)
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
                    Expect.equal (scale (newPitch A Sharp) Ionian)
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
                    Expect.equal (scale (newPitch B Flat) Ionian)
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
                    Expect.equal (scale (newPitch B Natural) Ionian)
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
