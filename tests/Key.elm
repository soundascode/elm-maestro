module Key exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Chord exposing (Type(..), newChord)
import Maestro.Key exposing (chords, newKey, extendedChords)
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass exposing (PitchClass(..))
import Maestro.Quality exposing (Quality(..))
import Maestro.Scale exposing (Scale(..))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Key Test Suite"
        [ describe "chords in key tests"
            [ test "triads in the key of C" <|
                \_ ->
                    newKey (newPitch C Natural)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch C Natural) MajorTriad
                            , newChord (newPitch D Natural) MinorTriad
                            , newChord (newPitch E Natural) MinorTriad
                            , newChord (newPitch F Natural) MajorTriad
                            , newChord (newPitch G Natural) MajorTriad
                            , newChord (newPitch A Natural) MinorTriad
                            , newChord (newPitch B Natural) DiminishedTriad
                            ]
            , test "triads in the key of D" <|
                \_ ->
                    newKey (newPitch D Natural)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch D Natural) MajorTriad
                            , newChord (newPitch E Natural) MinorTriad
                            , newChord (newPitch F Sharp) MinorTriad
                            , newChord (newPitch G Natural) MajorTriad
                            , newChord (newPitch A Natural) MajorTriad
                            , newChord (newPitch B Natural) MinorTriad
                            , newChord (newPitch C Sharp) DiminishedTriad
                            ]
            , test "triads in the key of E" <|
                \_ ->
                    newKey (newPitch E Natural)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch E Natural) MajorTriad
                            , newChord (newPitch F Sharp) MinorTriad
                            , newChord (newPitch G Sharp) MinorTriad
                            , newChord (newPitch A Natural) MajorTriad
                            , newChord (newPitch B Natural) MajorTriad
                            , newChord (newPitch C Sharp) MinorTriad
                            , newChord (newPitch D Sharp) DiminishedTriad
                            ]
            , test "triads in the key of F" <|
                \_ ->
                    newKey (newPitch F Natural)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch F Natural) MajorTriad
                            , newChord (newPitch G Natural) MinorTriad
                            , newChord (newPitch A Natural) MinorTriad
                            , newChord (newPitch B Flat) MajorTriad
                            , newChord (newPitch C Natural) MajorTriad
                            , newChord (newPitch D Natural) MinorTriad
                            , newChord (newPitch E Natural) DiminishedTriad
                            ]
            , test "triads in the key of G" <|
                \_ ->
                    newKey (newPitch G Natural)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch G Natural) MajorTriad
                            , newChord (newPitch A Natural) MinorTriad
                            , newChord (newPitch B Natural) MinorTriad
                            , newChord (newPitch C Natural) MajorTriad
                            , newChord (newPitch D Natural) MajorTriad
                            , newChord (newPitch E Natural) MinorTriad
                            , newChord (newPitch F Sharp) DiminishedTriad
                            ]
            , test "triads in the key of A" <|
                \_ ->
                    newKey (newPitch A Natural)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch A Natural) MajorTriad
                            , newChord (newPitch B Natural) MinorTriad
                            , newChord (newPitch C Sharp) MinorTriad
                            , newChord (newPitch D Natural) MajorTriad
                            , newChord (newPitch E Natural) MajorTriad
                            , newChord (newPitch F Sharp) MinorTriad
                            , newChord (newPitch G Sharp) DiminishedTriad
                            ]
            , test "triads in the key of B" <|
                \_ ->
                    newKey (newPitch B Natural)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch B Natural) MajorTriad
                            , newChord (newPitch C Sharp) MinorTriad
                            , newChord (newPitch D Sharp) MinorTriad
                            , newChord (newPitch E Natural) MajorTriad
                            , newChord (newPitch F Sharp) MajorTriad
                            , newChord (newPitch G Sharp) MinorTriad
                            , newChord (newPitch A Sharp) DiminishedTriad
                            ]
            , test "triads in the key of C#" <|
                \_ ->
                    newKey (newPitch C Sharp)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch C Sharp) MajorTriad
                            , newChord (newPitch D Sharp) MinorTriad
                            , newChord (newPitch E Sharp) MinorTriad
                            , newChord (newPitch F Sharp) MajorTriad
                            , newChord (newPitch G Sharp) MajorTriad
                            , newChord (newPitch A Sharp) MinorTriad
                            , newChord (newPitch B Sharp) DiminishedTriad
                            ]
            , test "triads in the key of Db" <|
                \_ ->
                    newKey (newPitch D Flat)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch D Flat) MajorTriad
                            , newChord (newPitch E Flat) MinorTriad
                            , newChord (newPitch F Natural) MinorTriad
                            , newChord (newPitch G Flat) MajorTriad
                            , newChord (newPitch A Flat) MajorTriad
                            , newChord (newPitch B Flat) MinorTriad
                            , newChord (newPitch C Natural) DiminishedTriad
                            ]
            , test "triads in the key of Eb" <|
                \_ ->
                    newKey (newPitch E Flat)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch E Flat) MajorTriad
                            , newChord (newPitch F Natural) MinorTriad
                            , newChord (newPitch G Natural) MinorTriad
                            , newChord (newPitch A Flat) MajorTriad
                            , newChord (newPitch B Flat) MajorTriad
                            , newChord (newPitch C Natural) MinorTriad
                            , newChord (newPitch D Natural) DiminishedTriad
                            ]
            , test "triads in the key of F#" <|
                \_ ->
                    newKey (newPitch F Sharp)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch F Sharp) MajorTriad
                            , newChord (newPitch G Sharp) MinorTriad
                            , newChord (newPitch A Sharp) MinorTriad
                            , newChord (newPitch B Natural) MajorTriad
                            , newChord (newPitch C Sharp) MajorTriad
                            , newChord (newPitch D Sharp) MinorTriad
                            , newChord (newPitch E Sharp) DiminishedTriad
                            ]
            , test "triads in the key of Gb" <|
                \_ ->
                    newKey (newPitch G Flat)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch G Flat) MajorTriad
                            , newChord (newPitch A Flat) MinorTriad
                            , newChord (newPitch B Flat) MinorTriad
                            , newChord (newPitch C Flat) MajorTriad
                            , newChord (newPitch D Flat) MajorTriad
                            , newChord (newPitch E Flat) MinorTriad
                            , newChord (newPitch F Natural) DiminishedTriad
                            ]
            , test "triads in the key of G#" <|
                \_ ->
                    newKey (newPitch G Sharp)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch G Sharp) MajorTriad
                            , newChord (newPitch A Sharp) MinorTriad
                            , newChord (newPitch B Sharp) MinorTriad
                            , newChord (newPitch C Sharp) MajorTriad
                            , newChord (newPitch D Sharp) MajorTriad
                            , newChord (newPitch E Sharp) MinorTriad
                            , newChord (newPitch F SharpSharp) DiminishedTriad
                            ]
            , test "triads in the key of Ab" <|
                \_ ->
                    newKey (newPitch A Flat)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch A Flat) MajorTriad
                            , newChord (newPitch B Flat) MinorTriad
                            , newChord (newPitch C Natural) MinorTriad
                            , newChord (newPitch D Flat) MajorTriad
                            , newChord (newPitch E Flat) MajorTriad
                            , newChord (newPitch F Natural) MinorTriad
                            , newChord (newPitch G Natural) DiminishedTriad
                            ]
            , test "triads in the key of Bb" <|
                \_ ->
                    newKey (newPitch B Flat)
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch B Flat) MajorTriad
                            , newChord (newPitch C Natural) MinorTriad
                            , newChord (newPitch D Natural) MinorTriad
                            , newChord (newPitch E Flat) MajorTriad
                            , newChord (newPitch F Natural) MajorTriad
                            , newChord (newPitch G Natural) MinorTriad
                            , newChord (newPitch A Natural) DiminishedTriad
                            ]
            ]
        , describe "extended chords in key tests"
            [ test "tetrads in the key of C" <|
                \_ ->
                    newKey (newPitch C Natural)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch C Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch D Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch E Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch F Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch G Natural) Maestro.Chord.DominantSeventh
                            , newChord (newPitch A Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch B Natural) Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of D" <|
                \_ ->
                    newKey (newPitch D Natural)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch D Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch E Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch F Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch G Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch A Natural) Maestro.Chord.DominantSeventh
                            , newChord (newPitch B Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch C Sharp)   Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of E" <|
                \_ ->
                    newKey (newPitch E Natural)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch E Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch F Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch G Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch A Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch B Natural) Maestro.Chord.DominantSeventh
                            , newChord (newPitch C Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch D Sharp)   Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of F" <|
                \_ ->
                    newKey (newPitch F Natural)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch F Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch G Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch A Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch B Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch C Natural) Maestro.Chord.DominantSeventh
                            , newChord (newPitch D Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch E Natural) Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of G" <|
                \_ ->
                    newKey (newPitch G Natural)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch G Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch A Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch B Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch C Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch D Natural) Maestro.Chord.DominantSeventh
                            , newChord (newPitch E Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch F Sharp)   Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of A" <|
                \_ ->
                    newKey (newPitch A Natural)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch A Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch B Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch C Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch D Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch E Natural) Maestro.Chord.DominantSeventh
                            , newChord (newPitch F Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch G Sharp)   Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of B" <|
                \_ ->
                    newKey (newPitch B Natural)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch B Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch C Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch D Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch E Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch F Sharp)   Maestro.Chord.DominantSeventh
                            , newChord (newPitch G Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch A Sharp)   Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of C#" <|
                \_ ->
                    newKey (newPitch C Sharp)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch C Sharp) Maestro.Chord.MajorSeventh
                            , newChord (newPitch D Sharp) Maestro.Chord.MinorSeventh
                            , newChord (newPitch E Sharp) Maestro.Chord.MinorSeventh
                            , newChord (newPitch F Sharp) Maestro.Chord.MajorSeventh
                            , newChord (newPitch G Sharp) Maestro.Chord.DominantSeventh
                            , newChord (newPitch A Sharp) Maestro.Chord.MinorSeventh
                            , newChord (newPitch B Sharp) Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of Db" <|
                \_ ->
                    newKey (newPitch D Flat)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch D Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch E Flat)    Maestro.Chord.MinorSeventh
                            , newChord (newPitch F Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch G Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch A Flat)    Maestro.Chord.DominantSeventh
                            , newChord (newPitch B Flat)    Maestro.Chord.MinorSeventh
                            , newChord (newPitch C Natural) Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of Eb" <|
                \_ ->
                    newKey (newPitch E Flat)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch E Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch F Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch G Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch A Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch B Flat)    Maestro.Chord.DominantSeventh
                            , newChord (newPitch C Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch D Natural) Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of F#" <|
                \_ ->
                    newKey (newPitch F Sharp)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch F Sharp)   Maestro.Chord.MajorSeventh
                            , newChord (newPitch G Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch A Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch B Natural) Maestro.Chord.MajorSeventh
                            , newChord (newPitch C Sharp)   Maestro.Chord.DominantSeventh
                            , newChord (newPitch D Sharp)   Maestro.Chord.MinorSeventh
                            , newChord (newPitch E Sharp)   Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of Gb" <|
                \_ ->
                    newKey (newPitch G Flat)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch G Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch A Flat)    Maestro.Chord.MinorSeventh
                            , newChord (newPitch B Flat)    Maestro.Chord.MinorSeventh
                            , newChord (newPitch C Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch D Flat)    Maestro.Chord.DominantSeventh
                            , newChord (newPitch E Flat)    Maestro.Chord.MinorSeventh
                            , newChord (newPitch F Natural) Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of G#" <|
                \_ ->
                    newKey (newPitch G Sharp)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch G Sharp)      Maestro.Chord.MajorSeventh
                            , newChord (newPitch A Sharp)      Maestro.Chord.MinorSeventh
                            , newChord (newPitch B Sharp)      Maestro.Chord.MinorSeventh
                            , newChord (newPitch C Sharp)      Maestro.Chord.MajorSeventh
                            , newChord (newPitch D Sharp)      Maestro.Chord.DominantSeventh
                            , newChord (newPitch E Sharp)      Maestro.Chord.MinorSeventh
                            , newChord (newPitch F SharpSharp) Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of Ab" <|
                \_ ->
                    newKey (newPitch A Flat)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch A Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch B Flat)    Maestro.Chord.MinorSeventh
                            , newChord (newPitch C Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch D Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch E Flat)    Maestro.Chord.DominantSeventh
                            , newChord (newPitch F Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch G Natural) Maestro.Chord.HalfDiminishedSeventh
                            ]
            , test "tetrads in the key of Bb" <|
                \_ ->
                    newKey (newPitch B Flat)
                        |> extendedChords
                        |> Expect.equal
                            [ newChord (newPitch B Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch C Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch D Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch E Flat)    Maestro.Chord.MajorSeventh
                            , newChord (newPitch F Natural) Maestro.Chord.DominantSeventh
                            , newChord (newPitch G Natural) Maestro.Chord.MinorSeventh
                            , newChord (newPitch A Natural) Maestro.Chord.HalfDiminishedSeventh
                            ]
            ]
        ]
