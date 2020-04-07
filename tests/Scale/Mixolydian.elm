module Scale.Mixolydian exposing (all)

import Expect
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Note exposing (Note)
import Maestro.PitchClass exposing (Adjustment(..), PitchClass(..), newPitch)
import Maestro.Scale exposing (Mode(..), Scale, scale)
import Test exposing (..)


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Mixolydian scale tests"
            [ test "C Mixolydian" <|
                \() ->
                    Expect.equal (scale (newPitch C Natural) Mixolydian)
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
                    Expect.equal (scale (newPitch C Sharp) Mixolydian)
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
                    Expect.equal (scale (newPitch D Flat) Mixolydian)
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
                    Expect.equal (scale (newPitch D Natural) Mixolydian)
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
                    Expect.equal (scale (newPitch D Sharp) Mixolydian)
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
                    Expect.equal (scale (newPitch E Flat) Mixolydian)
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
                    Expect.equal (scale (newPitch E Natural) Mixolydian)
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
                    Expect.equal (scale (newPitch E Sharp) Mixolydian)
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
                    Expect.equal (scale (newPitch F Flat) Mixolydian)
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
                    Expect.equal (scale (newPitch F Natural) Mixolydian)
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
                    Expect.equal (scale (newPitch F Sharp) Mixolydian)
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
                    Expect.equal (scale (newPitch G Flat) Mixolydian)
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
                    Expect.equal (scale (newPitch G Natural) Mixolydian)
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
                    Expect.equal (scale (newPitch G Sharp) Mixolydian)
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
                    Expect.equal (scale (newPitch A Flat) Mixolydian)
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
                    Expect.equal (scale (newPitch A Natural) Mixolydian)
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
                    Expect.equal (scale (newPitch A Sharp) Mixolydian)
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
                    Expect.equal (scale (newPitch B Flat) Mixolydian)
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
                    Expect.equal (scale (newPitch B Natural) Mixolydian)
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
