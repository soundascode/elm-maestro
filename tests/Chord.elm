module Chord exposing (all)

import Expect
import Maestro.Chord exposing (Quality(..), chord, inversion)
import Maestro.PitchClass exposing (Adjustment(..), PitchClass(..), Tone, newTone)
import Test exposing (..)


all : Test
all =
    describe "Chord Test Suite"
        [ describe "Inversions tests"
            [ test "first inversion of major triad" <|
                \_ ->
                    chord (newTone C Natural) MajorTriad
                        |> inversion 1
                        |> Expect.equal
                            [ newTone C Natural
                            , newTone E Natural
                            , newTone G Natural
                            ]
            , test "second inversion of major triad" <|
                \_ ->
                    chord (newTone C Natural) MajorTriad
                        |> inversion 2
                        |> Expect.equal
                            [ newTone E Natural
                            , newTone G Natural
                            , newTone C Natural
                            ]
            , test "third inversion of major triad" <|
                \_ ->
                    chord (newTone C Natural) MajorTriad
                        |> inversion 3
                        |> Expect.equal
                            [ newTone G Natural
                            , newTone C Natural
                            , newTone E Natural
                            ]
            ]
        , describe "Major chords tests"
            [ test "C Major" <|
                \() ->
                    Expect.equal (chord (newTone C Natural) MajorTriad)
                        [ newTone C Natural
                        , newTone E Natural
                        , newTone G Natural
                        ]
            , test "C# Major" <|
                \() ->
                    Expect.equal (chord (newTone C Sharp) MajorTriad)
                        [ newTone C Sharp
                        , newTone E Sharp
                        , newTone G Sharp
                        ]
            , test "D Major" <|
                \() ->
                    Expect.equal (chord (newTone D Natural) MajorTriad)
                        [ newTone D Natural
                        , newTone F Sharp
                        , newTone A Natural
                        ]
            , test "D# Major" <|
                \() ->
                    Expect.equal (chord (newTone D Sharp) MajorTriad)
                        [ newTone D Sharp
                        , newTone F SharpSharp
                        , newTone A Sharp
                        ]
            , test "E Major" <|
                \() ->
                    Expect.equal (chord (newTone E Natural) MajorTriad)
                        [ newTone E Natural
                        , newTone G Sharp
                        , newTone B Natural
                        ]
            , test "F Major" <|
                \() ->
                    Expect.equal (chord (newTone F Natural) MajorTriad)
                        [ newTone F Natural
                        , newTone A Natural
                        , newTone C Natural
                        ]
            , test "F# Major" <|
                \() ->
                    Expect.equal (chord (newTone F Sharp) MajorTriad)
                        [ newTone F Sharp
                        , newTone A Sharp
                        , newTone C Sharp
                        ]
            , test "G Major" <|
                \() ->
                    Expect.equal (chord (newTone G Natural) MajorTriad)
                        [ newTone G Natural
                        , newTone B Natural
                        , newTone D Natural
                        ]
            , test "G# Major" <|
                \() ->
                    Expect.equal (chord (newTone G Sharp) MajorTriad)
                        [ newTone G Sharp
                        , newTone B Sharp
                        , newTone D Sharp
                        ]
            , test "A Major" <|
                \() ->
                    Expect.equal (chord (newTone A Natural) MajorTriad)
                        [ newTone A Natural
                        , newTone C Sharp
                        , newTone E Natural
                        ]
            , test "A# Major" <|
                \() ->
                    Expect.equal (chord (newTone A Sharp) MajorTriad)
                        [ newTone A Sharp
                        , newTone C SharpSharp
                        , newTone E Sharp
                        ]
            , test "B Major" <|
                \() ->
                    Expect.equal (chord (newTone B Natural) MajorTriad)
                        [ newTone B Natural
                        , newTone D Sharp
                        , newTone F Sharp
                        ]
            ]
        , describe "Minor chords tests"
            [ test "C minor" <|
                \() ->
                    Expect.equal (chord (newTone C Natural) MinorTriad)
                        [ newTone C Natural
                        , newTone E Flat
                        , newTone G Natural
                        ]
            , test "Db minor" <|
                \() ->
                    Expect.equal (chord (newTone D Flat) MinorTriad)
                        [ newTone D Flat
                        , newTone F Flat
                        , newTone A Flat
                        ]
            , test "D minor" <|
                \() ->
                    Expect.equal (chord (newTone D Natural) MinorTriad)
                        [ newTone D Natural
                        , newTone F Natural
                        , newTone A Natural
                        ]
            , test "Eb minor" <|
                \() ->
                    Expect.equal (chord (newTone E Flat) MinorTriad)
                        [ newTone E Flat
                        , newTone G Flat
                        , newTone B Flat
                        ]
            , test "E minor" <|
                \() ->
                    Expect.equal (chord (newTone E Natural) MinorTriad)
                        [ newTone E Natural
                        , newTone G Natural
                        , newTone B Natural
                        ]
            , test "F minor" <|
                \() ->
                    Expect.equal (chord (newTone F Natural) MinorTriad)
                        [ newTone F Natural
                        , newTone A Flat
                        , newTone C Natural
                        ]
            , test "Gb minor" <|
                \() ->
                    Expect.equal (chord (newTone G Flat) MinorTriad)
                        [ newTone G Flat
                        , newTone B FlatFlat
                        , newTone D Flat
                        ]
            , test "G minor" <|
                \() ->
                    Expect.equal (chord (newTone G Natural) MinorTriad)
                        [ newTone G Natural
                        , newTone B Flat
                        , newTone D Natural
                        ]
            , test "Ab minor" <|
                \() ->
                    Expect.equal (chord (newTone A Flat) MinorTriad)
                        [ newTone A Flat
                        , newTone C Flat
                        , newTone E Flat
                        ]
            , test "A minor" <|
                \() ->
                    Expect.equal (chord (newTone A Natural) MinorTriad)
                        [ newTone A Natural
                        , newTone C Natural
                        , newTone E Natural
                        ]
            , test "Bb minor" <|
                \() ->
                    Expect.equal (chord (newTone B Flat) MinorTriad)
                        [ newTone B Flat
                        , newTone D Flat
                        , newTone F Natural
                        ]
            , test "B minor" <|
                \() ->
                    Expect.equal (chord (newTone B Natural) MinorTriad)
                        [ newTone B Natural
                        , newTone D Natural
                        , newTone F Sharp
                        ]
            ]
        ]
