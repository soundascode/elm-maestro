module Scale.Lydian exposing (all)

import Expect
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Note exposing (Note)
import Maestro.PitchClass exposing (Accidental(..), PitchClass(..), newPitch)
import Maestro.Scale exposing (Mode(..), Scale, scale)
import Test exposing (..)


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Lydian scale tests"
            [ test "C Lydian" <|
                \() ->
                    Expect.equal (scale (newPitch C Natural) Lydian)
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
                    Expect.equal (scale (newPitch C Sharp) Lydian)
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
                    Expect.equal (scale (newPitch D Flat) Lydian)
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
                    Expect.equal (scale (newPitch D Natural) Lydian)
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
                    Expect.equal (scale (newPitch D Sharp) Lydian)
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
                    Expect.equal (scale (newPitch E Flat) Lydian)
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
                    Expect.equal (scale (newPitch E Natural) Lydian)
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
                    Expect.equal (scale (newPitch E Sharp) Lydian)
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
                    Expect.equal (scale (newPitch F Flat) Lydian)
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
                    Expect.equal (scale (newPitch F Natural) Lydian)
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
                    Expect.equal (scale (newPitch F Sharp) Lydian)
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
                    Expect.equal (scale (newPitch G Flat) Lydian)
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
                    Expect.equal (scale (newPitch G Natural) Lydian)
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
                    Expect.equal (scale (newPitch G Sharp) Lydian)
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
                    Expect.equal (scale (newPitch A Flat) Lydian)
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
                    Expect.equal (scale (newPitch A Natural) Lydian)
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
                    Expect.equal (scale (newPitch A Sharp) Lydian)
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
                    Expect.equal (scale (newPitch B Flat) Lydian)
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
                    Expect.equal (scale (newPitch B Natural) Lydian)
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
