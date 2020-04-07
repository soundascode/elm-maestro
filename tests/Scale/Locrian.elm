module Scale.Locrian exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Note exposing (Note)
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass exposing (PitchClass(..))
import Maestro.Scale exposing (Mode(..), Scale, scale)
import String
import Test exposing (..)


all : Test
all =
    describe "Scale Test Suite"
        [ describe "locrian scale tests"
            [ test "C locrian" <|
                \() ->
                    Expect.equal (scale (newPitch C Natural) Locrian)
                        [ newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        ]
            , test "C# locrian" <|
                \() ->
                    Expect.equal (scale (newPitch C Sharp) Locrian)
                        [ newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        ]
            , test "Db locrian" <|
                \() ->
                    Expect.equal (scale (newPitch D Flat) Locrian)
                        [ newPitch D Flat
                        , newPitch E FlatFlat
                        , newPitch F Flat
                        , newPitch G Flat
                        , newPitch A FlatFlat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        ]
            , test "D locrian" <|
                \() ->
                    Expect.equal (scale (newPitch D Natural) Locrian)
                        [ newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        ]
            , test "D# locrian" <|
                \() ->
                    Expect.equal (scale (newPitch D Sharp) Locrian)
                        [ newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        ]
            , test "Eb locrian" <|
                \() ->
                    Expect.equal (scale (newPitch E Flat) Locrian)
                        [ newPitch E Flat
                        , newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        ]
            , test "E locrian" <|
                \() ->
                    Expect.equal (scale (newPitch E Natural) Locrian)
                        [ newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        ]
            , test "E# locrian" <|
                \() ->
                    Expect.equal (scale (newPitch E Sharp) Locrian)
                        [ newPitch E Sharp
                        , newPitch F Sharp
                        , newPitch G Sharp
                        , newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        ]
            , test "Fb locrian" <|
                \() ->
                    Expect.equal (scale (newPitch F Flat) Locrian)
                        [ newPitch F Flat
                        , newPitch G FlatFlat
                        , newPitch A FlatFlat
                        , newPitch B FlatFlat
                        , newPitch C FlatFlat
                        , newPitch D FlatFlat
                        , newPitch E FlatFlat
                        ]
            , test "F locrian" <|
                \() ->
                    Expect.equal (scale (newPitch F Natural) Locrian)
                        [ newPitch F Natural
                        , newPitch G Flat
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        ]
            , test "F# locrian" <|
                \() ->
                    Expect.equal (scale (newPitch F Sharp) Locrian)
                        [ newPitch F Sharp
                        , newPitch G Natural
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        ]
            , test "Gb locrian" <|
                \() ->
                    Expect.equal (scale (newPitch G Flat) Locrian)
                        [ newPitch G Flat
                        , newPitch A FlatFlat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D FlatFlat
                        , newPitch E FlatFlat
                        , newPitch F Flat
                        ]
            , test "G locrian" <|
                \() ->
                    Expect.equal (scale (newPitch G Natural) Locrian)
                        [ newPitch G Natural
                        , newPitch A Flat
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Natural
                        ]
            , test "G# locrian" <|
                \() ->
                    Expect.equal (scale (newPitch G Sharp) Locrian)
                        [ newPitch G Sharp
                        , newPitch A Natural
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Sharp
                        ]
            , test "Ab locrian" <|
                \() ->
                    Expect.equal (scale (newPitch A Flat) Locrian)
                        [ newPitch A Flat
                        , newPitch B FlatFlat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E FlatFlat
                        , newPitch F Flat
                        , newPitch G Flat
                        ]
            , test "A locrian" <|
                \() ->
                    Expect.equal (scale (newPitch A Natural) Locrian)
                        [ newPitch A Natural
                        , newPitch B Flat
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Flat
                        , newPitch F Natural
                        , newPitch G Natural
                        ]
            , test "A# locrian" <|
                \() ->
                    Expect.equal (scale (newPitch A Sharp) Locrian)
                        [ newPitch A Sharp
                        , newPitch B Natural
                        , newPitch C Sharp
                        , newPitch D Sharp
                        , newPitch E Natural
                        , newPitch F Sharp
                        , newPitch G Sharp
                        ]
            , test "Bb locrian" <|
                \() ->
                    Expect.equal (scale (newPitch B Flat) Locrian)
                        [ newPitch B Flat
                        , newPitch C Flat
                        , newPitch D Flat
                        , newPitch E Flat
                        , newPitch F Flat
                        , newPitch G Flat
                        , newPitch A Flat
                        ]
            , test "B locrian" <|
                \() ->
                    Expect.equal (scale (newPitch B Natural) Locrian)
                        [ newPitch B Natural
                        , newPitch C Natural
                        , newPitch D Natural
                        , newPitch E Natural
                        , newPitch F Natural
                        , newPitch G Natural
                        , newPitch A Natural
                        ]
            ]
        ]
