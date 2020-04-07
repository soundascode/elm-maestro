module Scale.Phrygian exposing (all)

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
    describe "Phrygian test suite"
        [ describe "phrygian mode tests"
            [ test "C Phrygian" <|
                \() ->
                    Expect.equal (scale (newPitch C Natural) Phrygian)
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
                    Expect.equal (scale (newPitch C Sharp) Phrygian)
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
                    Expect.equal (scale (newPitch D Flat) Phrygian)
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
                    Expect.equal (scale (newPitch D Natural) Phrygian)
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
                    Expect.equal (scale (newPitch D Sharp) Phrygian)
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
                    Expect.equal (scale (newPitch E Flat) Phrygian)
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
                    Expect.equal (scale (newPitch E Natural) Phrygian)
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
                    Expect.equal (scale (newPitch E Sharp) Phrygian)
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
                    Expect.equal (scale (newPitch F Flat) Phrygian)
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
                    Expect.equal (scale (newPitch F Natural) Phrygian)
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
                    Expect.equal (scale (newPitch F Sharp) Phrygian)
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
                    Expect.equal (scale (newPitch G Flat) Phrygian)
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
                    Expect.equal (scale (newPitch G Natural) Phrygian)
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
                    Expect.equal (scale (newPitch G Sharp) Phrygian)
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
                    Expect.equal (scale (newPitch A Flat) Phrygian)
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
                    Expect.equal (scale (newPitch A Natural) Phrygian)
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
                    Expect.equal (scale (newPitch A Sharp) Phrygian)
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
                    Expect.equal (scale (newPitch B Flat) Phrygian)
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
                    Expect.equal (scale (newPitch B Natural) Phrygian)
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
