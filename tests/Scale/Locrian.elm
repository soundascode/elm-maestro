module Scale.Locrian exposing (all)

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
        [ describe "locrian scale tests"
            [ test "C locrian" <|
                \() ->
                    pitches (newPitch C Natural) Locrian
                    |> Expect.equal
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
                    pitches (newPitch C Sharp) Locrian
                    |> Expect.equal
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
                    pitches (newPitch D Flat) Locrian
                    |> Expect.equal
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
                    pitches (newPitch D Natural) Locrian
                    |> Expect.equal
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
                    pitches (newPitch D Sharp) Locrian
                    |> Expect.equal
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
                    pitches (newPitch E Flat) Locrian
                    |> Expect.equal
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
                    pitches (newPitch E Natural) Locrian
                    |> Expect.equal
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
                    pitches (newPitch E Sharp) Locrian
                    |> Expect.equal
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
                    pitches (newPitch F Flat) Locrian
                    |> Expect.equal
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
                    pitches (newPitch F Natural) Locrian
                    |> Expect.equal
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
                    pitches (newPitch F Sharp) Locrian
                    |> Expect.equal
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
                    pitches (newPitch G Flat) Locrian
                    |> Expect.equal
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
                    pitches (newPitch G Natural) Locrian
                    |> Expect.equal
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
                    pitches (newPitch G Sharp) Locrian
                    |> Expect.equal
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
                    pitches (newPitch A Flat) Locrian
                    |> Expect.equal
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
                    pitches (newPitch A Natural) Locrian
                    |> Expect.equal
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
                    pitches (newPitch A Sharp) Locrian
                    |> Expect.equal
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
                    pitches (newPitch B Flat) Locrian
                    |> Expect.equal
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
                    pitches (newPitch B Natural) Locrian
                    |> Expect.equal
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
