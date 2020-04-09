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
                    newChord (newPitch C Natural) Major
                        |> inversion 1
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            ]
            , test "second inversion of major triad" <|
                \_ ->
                    newChord (newPitch C Natural) Major
                        |> inversion 2
                        |> Expect.equal
                            [ newPitch E Natural
                            , newPitch G Natural
                            , newPitch C Natural
                            ]
            , test "third inversion of major triad" <|
                \_ ->
                    newChord (newPitch C Natural) Major
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
                    newChord (newPitch C Natural) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            ]
            , test "C# Major" <|
                \() ->
                    newChord (newPitch C Sharp) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Sharp
                            , newPitch E Sharp
                            , newPitch G Sharp
                            ]
            , test "D Major" <|
                \() ->
                    newChord (newPitch D Natural) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch D Natural
                            , newPitch F Sharp
                            , newPitch A Natural
                            ]
            , test "D# Major" <|
                \() ->
                    newChord (newPitch D Sharp) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch D Sharp
                            , newPitch F SharpSharp
                            , newPitch A Sharp
                            ]
            , test "E Major" <|
                \() ->
                    newChord (newPitch E Natural) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch E Natural
                            , newPitch G Sharp
                            , newPitch B Natural
                            ]
            , test "F Major" <|
                \() ->
                    newChord (newPitch F Natural) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch F Natural
                            , newPitch A Natural
                            , newPitch C Natural
                            ]
            , test "F# Major" <|
                \() ->
                    newChord (newPitch F Sharp) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch F Sharp
                            , newPitch A Sharp
                            , newPitch C Sharp
                            ]
            , test "G Major" <|
                \() ->
                    newChord (newPitch G Natural) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch G Natural
                            , newPitch B Natural
                            , newPitch D Natural
                            ]
            , test "G# Major" <|
                \() ->
                    newChord (newPitch G Sharp) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch G Sharp
                            , newPitch B Sharp
                            , newPitch D Sharp
                            ]
            , test "A Major" <|
                \() ->
                    newChord (newPitch A Natural) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch A Natural
                            , newPitch C Sharp
                            , newPitch E Natural
                            ]
            , test "A# Major" <|
                \() ->
                    newChord (newPitch A Sharp) Major
                        |> pitches
                        |> Expect.equal
                            [ newPitch A Sharp
                            , newPitch C SharpSharp
                            , newPitch E Sharp
                            ]
            , test "B Major" <|
                \() ->
                    newChord (newPitch B Natural) Major
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
                    newChord (newPitch C Natural) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Flat
                            , newPitch G Natural
                            ]
            , test "Db minor" <|
                \() ->
                    newChord (newPitch D Flat) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch D Flat
                            , newPitch F Flat
                            , newPitch A Flat
                            ]
            , test "D minor" <|
                \() ->
                    newChord (newPitch D Natural) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch D Natural
                            , newPitch F Natural
                            , newPitch A Natural
                            ]
            , test "Eb minor" <|
                \() ->
                    newChord (newPitch E Flat) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch E Flat
                            , newPitch G Flat
                            , newPitch B Flat
                            ]
            , test "E minor" <|
                \() ->
                    newChord (newPitch E Natural) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Natural
                            ]
            , test "F minor" <|
                \() ->
                    newChord (newPitch F Natural) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch F Natural
                            , newPitch A Flat
                            , newPitch C Natural
                            ]
            , test "Gb minor" <|
                \() ->
                    newChord (newPitch G Flat) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch G Flat
                            , newPitch B FlatFlat
                            , newPitch D Flat
                            ]
            , test "G minor" <|
                \() ->
                    newChord (newPitch G Natural) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Natural
                            ]
            , test "Ab minor" <|
                \() ->
                    newChord (newPitch A Flat) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch A Flat
                            , newPitch C Flat
                            , newPitch E Flat
                            ]
            , test "A minor" <|
                \() ->
                    newChord (newPitch A Natural) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch A Natural
                            , newPitch C Natural
                            , newPitch E Natural
                            ]
            , test "Bb minor" <|
                \() ->
                    newChord (newPitch B Flat) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch B Flat
                            , newPitch D Flat
                            , newPitch F Natural
                            ]
            , test "B minor" <|
                \() ->
                    newChord (newPitch B Natural) Minor
                        |> pitches
                        |> Expect.equal
                            [ newPitch B Natural
                            , newPitch D Natural
                            , newPitch F Sharp
                            ]
            ]
        ]
