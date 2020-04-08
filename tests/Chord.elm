module Chord exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Chord exposing (inversion, newChord, pitches)
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass exposing (PitchClass(..))
import Maestro.Quality exposing (Quality(..))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Chord Test Suite"
        [ describe "Inversions tests"
            [ test "first inversion of major triad" <|
                \_ ->
                    newChord (newPitch C Natural) MajorTriad
                        |> inversion 1
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            ]
            , test "second inversion of major triad" <|
                \_ ->
                    newChord (newPitch C Natural) MajorTriad
                        |> inversion 2
                        |> Expect.equal
                            [ newPitch E Natural
                            , newPitch G Natural
                            , newPitch C Natural
                            ]
            , test "third inversion of major triad" <|
                \_ ->
                    newChord (newPitch C Natural) MajorTriad
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
                    newChord (newPitch C Natural) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            ]
            , test "C# Major" <|
                \() ->
                    newChord (newPitch C Sharp) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Sharp
                            , newPitch E Sharp
                            , newPitch G Sharp
                            ]
            , test "D Major" <|
                \() ->
                    newChord (newPitch D Natural) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch D Natural
                            , newPitch F Sharp
                            , newPitch A Natural
                            ]
            , test "D# Major" <|
                \() ->
                    newChord (newPitch D Sharp) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch D Sharp
                            , newPitch F SharpSharp
                            , newPitch A Sharp
                            ]
            , test "E Major" <|
                \() ->
                    newChord (newPitch E Natural) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch E Natural
                            , newPitch G Sharp
                            , newPitch B Natural
                            ]
            , test "F Major" <|
                \() ->
                    newChord (newPitch F Natural) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch F Natural
                            , newPitch A Natural
                            , newPitch C Natural
                            ]
            , test "F# Major" <|
                \() ->
                    newChord (newPitch F Sharp) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch F Sharp
                            , newPitch A Sharp
                            , newPitch C Sharp
                            ]
            , test "G Major" <|
                \() ->
                    newChord (newPitch G Natural) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch G Natural
                            , newPitch B Natural
                            , newPitch D Natural
                            ]
            , test "G# Major" <|
                \() ->
                    newChord (newPitch G Sharp) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch G Sharp
                            , newPitch B Sharp
                            , newPitch D Sharp
                            ]
            , test "A Major" <|
                \() ->
                    newChord (newPitch A Natural) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch A Natural
                            , newPitch C Sharp
                            , newPitch E Natural
                            ]
            , test "A# Major" <|
                \() ->
                    newChord (newPitch A Sharp) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch A Sharp
                            , newPitch C SharpSharp
                            , newPitch E Sharp
                            ]
            , test "B Major" <|
                \() ->
                    newChord (newPitch B Natural) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch B Natural
                            , newPitch D Sharp
                            , newPitch F Sharp
                            ]
            ]
        , describe "Minor chords tests"
            [ test "C minor" <|
                \() ->
                    newChord (newPitch C Natural) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Flat
                            , newPitch G Natural
                            ]
            , test "Db minor" <|
                \() ->
                    newChord (newPitch D Flat) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch D Flat
                            , newPitch F Flat
                            , newPitch A Flat
                            ]
            , test "D minor" <|
                \() ->
                    newChord (newPitch D Natural) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch D Natural
                            , newPitch F Natural
                            , newPitch A Natural
                            ]
            , test "Eb minor" <|
                \() ->
                    newChord (newPitch E Flat) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch E Flat
                            , newPitch G Flat
                            , newPitch B Flat
                            ]
            , test "E minor" <|
                \() ->
                    newChord (newPitch E Natural) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Natural
                            ]
            , test "F minor" <|
                \() ->
                    newChord (newPitch F Natural) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch F Natural
                            , newPitch A Flat
                            , newPitch C Natural
                            ]
            , test "Gb minor" <|
                \() ->
                    newChord (newPitch G Flat) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch G Flat
                            , newPitch B FlatFlat
                            , newPitch D Flat
                            ]
            , test "G minor" <|
                \() ->
                    newChord (newPitch G Natural) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Natural
                            ]
            , test "Ab minor" <|
                \() ->
                    newChord (newPitch A Flat) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch A Flat
                            , newPitch C Flat
                            , newPitch E Flat
                            ]
            , test "A minor" <|
                \() ->
                    newChord (newPitch A Natural) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch A Natural
                            , newPitch C Natural
                            , newPitch E Natural
                            ]
            , test "Bb minor" <|
                \() ->
                    newChord (newPitch B Flat) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch B Flat
                            , newPitch D Flat
                            , newPitch F Natural
                            ]
            , test "B minor" <|
                \() ->
                    newChord (newPitch B Natural) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch B Natural
                            , newPitch D Natural
                            , newPitch F Sharp
                            ]
            ]
        ]
