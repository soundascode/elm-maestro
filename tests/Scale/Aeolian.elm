module Scale.Aeolian exposing (all)

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
        [ describe "minor scale tests"
            [ test "C minor" <|
                \() ->
                    Expect.equal (scale (newPitch C Natural) Aeolian)
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
                    Expect.equal (scale (newPitch C Sharp) Aeolian)
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
                    Expect.equal (scale (newPitch D Natural) Aeolian)
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
                    Expect.equal (scale (newPitch D Sharp) Aeolian)
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
                    Expect.equal (scale (newPitch E Natural) Aeolian)
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
                    Expect.equal (scale (newPitch E Sharp) Aeolian)
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
                    Expect.equal (scale (newPitch F Natural) Aeolian)
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
                    Expect.equal (scale (newPitch F Sharp) Aeolian)
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
                    Expect.equal (scale (newPitch G Natural) Aeolian)
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
                    Expect.equal (scale (newPitch G Sharp) Aeolian)
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
                    Expect.equal (scale (newPitch A Natural) Aeolian)
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
                    Expect.equal (scale (newPitch A Sharp) Aeolian)
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
                    Expect.equal (scale (newPitch B Natural) Aeolian)
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
