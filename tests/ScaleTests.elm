module ScaleTests exposing (all)

import Maestro.Note exposing (Note)
import Maestro.Pitch exposing (Key(..), Adjustment(..), newPitch)
import Maestro.Interval exposing (Interval(..), Degree(..))
import Maestro.Scale exposing (Scale, Mode(..), scale)
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Major scale tests"
            [ test "C Major" <|
                \() ->
                    Expect.equal (scale (newPitch C Natural) Major)
                        [ newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        ]
            , test "C# Major" <|
                \() ->
                    Expect.equal (scale (newPitch C Sharp) Major)
                        [ newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        ]
            , test "D Major" <|
                \() ->
                    Expect.equal (scale (newPitch D Natural) Major)
                        [ newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        ]
            , test "D# Major" <|
                \() ->
                    Expect.equal (scale (newPitch D Sharp) Major)
                        [ newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        ]
            , test "E Major" <|
                \() ->
                    Expect.equal (scale (newPitch E Natural) Major)
                        [ newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        ]
            , test "Fb Major" <|
                \() ->
                    Expect.equal (scale (newPitch F Flat) Major)
                        [ newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        ]
            , test "F Major" <|
                \() ->
                    Expect.equal (scale (newPitch F Natural) Major)
                        [ newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        ]
            , test "F# Major" <|
                \() ->
                    Expect.equal (scale (newPitch F Sharp) Major)
                        [ newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        ]
            , test "G Major" <|
                \() ->
                    Expect.equal (scale (newPitch G Natural) Major)
                        [ newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        ]
            , test "G# Major" <|
                \() ->
                    Expect.equal (scale (newPitch G Sharp) Major)
                        [ newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        ]
            , test "A Major" <|
                \() ->
                    Expect.equal (scale (newPitch A Natural) Major)
                        [ newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        ]
            , test "A# Major" <|
                \() ->
                    Expect.equal (scale (newPitch A Sharp) Major)
                        [ newPitch A Sharp
                        , newPitch B Sharp
                        , newPitch C SharpSharp
                        , newPitch D Sharp
                        , newPitch E Sharp
                        , newPitch F SharpSharp
                        , newPitch G SharpSharp
                        ]
            , test "B Major" <|
                \() ->
                    Expect.equal (scale (newPitch B Natural) Major)
                        [ newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        ]
            ]
        , describe "minor scale tests"
            [ test "C minor" <|
                \() ->
                    Expect.equal (scale (newPitch C Natural) Minor)
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
                    Expect.equal (scale (newPitch C Sharp) Minor)
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
                    Expect.equal (scale (newPitch D Natural) Minor)
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
                    Expect.equal (scale (newPitch D Sharp) Minor)
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
                    Expect.equal (scale (newPitch E Natural) Minor)
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
                    Expect.equal (scale (newPitch E Sharp) Minor)
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
                    Expect.equal (scale (newPitch F Natural) Minor)
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
                    Expect.equal (scale (newPitch F Sharp) Minor)
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
                    Expect.equal (scale (newPitch G Natural) Minor)
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
                    Expect.equal (scale (newPitch G Sharp) Minor)
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
                    Expect.equal (scale (newPitch A Natural) Minor)
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
                    Expect.equal (scale (newPitch A Sharp) Minor)
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
                    Expect.equal (scale (newPitch B Natural) Minor)
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
