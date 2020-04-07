module Scale.Dorian exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Note exposing (Note)
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass exposing (PitchClass(..))
import Maestro.Scale exposing (Mode(..), Scale, scale)
import Test exposing (..)


all : Test
all =
    describe "Dorian test suite"
        [ describe "dorian mode tests"
            [ test "C Dorian" <|
                \() ->
                    Expect.equal (scale (newPitch C Natural) Dorian)
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
                    Expect.equal (scale (newPitch C Sharp) Dorian)
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
                    Expect.equal (scale (newPitch D Flat) Dorian)
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
                    Expect.equal (scale (newPitch D Natural) Dorian)
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
                    Expect.equal (scale (newPitch D Sharp) Dorian)
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
                    Expect.equal (scale (newPitch E Flat) Dorian)
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
                    Expect.equal (scale (newPitch E Natural) Dorian)
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
                    Expect.equal (scale (newPitch E Sharp) Dorian)
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
                    Expect.equal (scale (newPitch F Flat) Dorian)
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
                    Expect.equal (scale (newPitch F Natural) Dorian)
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
                    Expect.equal (scale (newPitch F Sharp) Dorian)
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
                    Expect.equal (scale (newPitch G Flat) Dorian)
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
                    Expect.equal (scale (newPitch G Natural) Dorian)
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
                    Expect.equal (scale (newPitch G Sharp) Dorian)
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
                    Expect.equal (scale (newPitch A Flat) Dorian)
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
                    Expect.equal (scale (newPitch A Natural) Dorian)
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
                    Expect.equal (scale (newPitch A Sharp) Dorian)
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
                    Expect.equal (scale (newPitch B Flat) Dorian)
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
                    Expect.equal (scale (newPitch B Natural) Dorian)
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
