module Chord exposing (all)

import Expect
import Maestro.Chord exposing (Quality(..), chord, inversion)
import Maestro.PitchClass exposing (Accidental(..), Pitch, PitchClass(..), newPitch)
import Test exposing (..)


all : Test
all =
    describe "Chord Test Suite"
        [ describe "Inversions tests"
            [ test "first inversion of major triad" <|
                \_ ->
                    chord (newPitch C Natural) MajorTriad
                        |> inversion 1
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            ]
            , test "second inversion of major triad" <|
                \_ ->
                    chord (newPitch C Natural) MajorTriad
                        |> inversion 2
                        |> Expect.equal
                            [ newPitch E Natural
                            , newPitch G Natural
                            , newPitch C Natural
                            ]
            , test "third inversion of major triad" <|
                \_ ->
                    chord (newPitch C Natural) MajorTriad
                        |> inversion 3
                        |> Expect.equal
                            [ newPitch G Natural
                            , newPitch C Natural
                            , newPitch E Natural
                            ]
            ]
        , describe "Major chords tests"
            [ test "C Major" <|
                \() ->
                    Expect.equal (chord (newPitch C Natural) MajorTriad)
                        [ newPitch C Natural
                        , newPitch E Natural
                        , newPitch G Natural
                        ]
            , test "C# Major" <|
                \() ->
                    Expect.equal (chord (newPitch C Sharp) MajorTriad)
                        [ newPitch C Sharp
                        , newPitch E Sharp
                        , newPitch G Sharp
                        ]
            , test "D Major" <|
                \() ->
                    Expect.equal (chord (newPitch D Natural) MajorTriad)
                        [ newPitch D Natural
                        , newPitch F Sharp
                        , newPitch A Natural
                        ]
            , test "D# Major" <|
                \() ->
                    Expect.equal (chord (newPitch D Sharp) MajorTriad)
                        [ newPitch D Sharp
                        , newPitch F SharpSharp
                        , newPitch A Sharp
                        ]
            , test "E Major" <|
                \() ->
                    Expect.equal (chord (newPitch E Natural) MajorTriad)
                        [ newPitch E Natural
                        , newPitch G Sharp
                        , newPitch B Natural
                        ]
            , test "F Major" <|
                \() ->
                    Expect.equal (chord (newPitch F Natural) MajorTriad)
                        [ newPitch F Natural
                        , newPitch A Natural
                        , newPitch C Natural
                        ]
            , test "F# Major" <|
                \() ->
                    Expect.equal (chord (newPitch F Sharp) MajorTriad)
                        [ newPitch F Sharp
                        , newPitch A Sharp
                        , newPitch C Sharp
                        ]
            , test "G Major" <|
                \() ->
                    Expect.equal (chord (newPitch G Natural) MajorTriad)
                        [ newPitch G Natural
                        , newPitch B Natural
                        , newPitch D Natural
                        ]
            , test "G# Major" <|
                \() ->
                    Expect.equal (chord (newPitch G Sharp) MajorTriad)
                        [ newPitch G Sharp
                        , newPitch B Sharp
                        , newPitch D Sharp
                        ]
            , test "A Major" <|
                \() ->
                    Expect.equal (chord (newPitch A Natural) MajorTriad)
                        [ newPitch A Natural
                        , newPitch C Sharp
                        , newPitch E Natural
                        ]
            , test "A# Major" <|
                \() ->
                    Expect.equal (chord (newPitch A Sharp) MajorTriad)
                        [ newPitch A Sharp
                        , newPitch C SharpSharp
                        , newPitch E Sharp
                        ]
            , test "B Major" <|
                \() ->
                    Expect.equal (chord (newPitch B Natural) MajorTriad)
                        [ newPitch B Natural
                        , newPitch D Sharp
                        , newPitch F Sharp
                        ]
            ]
        , describe "Minor chords tests"
            [ test "C minor" <|
                \() ->
                    Expect.equal (chord (newPitch C Natural) MinorTriad)
                        [ newPitch C Natural
                        , newPitch E Flat
                        , newPitch G Natural
                        ]
            , test "Db minor" <|
                \() ->
                    Expect.equal (chord (newPitch D Flat) MinorTriad)
                        [ newPitch D Flat
                        , newPitch F Flat
                        , newPitch A Flat
                        ]
            , test "D minor" <|
                \() ->
                    Expect.equal (chord (newPitch D Natural) MinorTriad)
                        [ newPitch D Natural
                        , newPitch F Natural
                        , newPitch A Natural
                        ]
            , test "Eb minor" <|
                \() ->
                    Expect.equal (chord (newPitch E Flat) MinorTriad)
                        [ newPitch E Flat
                        , newPitch G Flat
                        , newPitch B Flat
                        ]
            , test "E minor" <|
                \() ->
                    Expect.equal (chord (newPitch E Natural) MinorTriad)
                        [ newPitch E Natural
                        , newPitch G Natural
                        , newPitch B Natural
                        ]
            , test "F minor" <|
                \() ->
                    Expect.equal (chord (newPitch F Natural) MinorTriad)
                        [ newPitch F Natural
                        , newPitch A Flat
                        , newPitch C Natural
                        ]
            , test "Gb minor" <|
                \() ->
                    Expect.equal (chord (newPitch G Flat) MinorTriad)
                        [ newPitch G Flat
                        , newPitch B FlatFlat
                        , newPitch D Flat
                        ]
            , test "G minor" <|
                \() ->
                    Expect.equal (chord (newPitch G Natural) MinorTriad)
                        [ newPitch G Natural
                        , newPitch B Flat
                        , newPitch D Natural
                        ]
            , test "Ab minor" <|
                \() ->
                    Expect.equal (chord (newPitch A Flat) MinorTriad)
                        [ newPitch A Flat
                        , newPitch C Flat
                        , newPitch E Flat
                        ]
            , test "A minor" <|
                \() ->
                    Expect.equal (chord (newPitch A Natural) MinorTriad)
                        [ newPitch A Natural
                        , newPitch C Natural
                        , newPitch E Natural
                        ]
            , test "Bb minor" <|
                \() ->
                    Expect.equal (chord (newPitch B Flat) MinorTriad)
                        [ newPitch B Flat
                        , newPitch D Flat
                        , newPitch F Natural
                        ]
            , test "B minor" <|
                \() ->
                    Expect.equal (chord (newPitch B Natural) MinorTriad)
                        [ newPitch B Natural
                        , newPitch D Natural
                        , newPitch F Sharp
                        ]
            ]
        ]
