module ScaleTests exposing (all)

import Note exposing (Note)
import Key exposing (Key(..), Adjustment(..), newTone)
import Interval exposing (Interval(..), Degree(..), substractDegree, diatonicDegreeOf, distance, addInterval)
import Scale exposing (Scale, Mode(..), scale)
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Major scale tests"
            [ test "C Major" <|
                \() ->
                    Expect.equal (scale (newTone C Natural) Major)
                        [ newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        ]
            , test "C# Major" <|
                \() ->
                    Expect.equal (scale (newTone C Sharp) Major)
                        [ newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        ]
            , test "D Major" <|
                \() ->
                    Expect.equal (scale (newTone D Natural) Major)
                        [ newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        ]
            , test "D# Major" <|
                \() ->
                    Expect.equal (scale (newTone D Sharp) Major)
                        [ newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        ]
            , test "E Major" <|
                \() ->
                    Expect.equal (scale (newTone E Natural) Major)
                        [ newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        ]
            , test "Fb Major" <|
                \() ->
                    Expect.equal (scale (newTone F Flat) Major)
                        [ newTone F Flat
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        ]
            , test "F Major" <|
                \() ->
                    Expect.equal (scale (newTone F Natural) Major)
                        [ newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        ]
            , test "F# Major" <|
                \() ->
                    Expect.equal (scale (newTone F Sharp) Major)
                        [ newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        ]
            , test "G Major" <|
                \() ->
                    Expect.equal (scale (newTone G Natural) Major)
                        [ newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        ]
            , test "G# Major" <|
                \() ->
                    Expect.equal (scale (newTone G Sharp) Major)
                        [ newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        ]
            , test "A Major" <|
                \() ->
                    Expect.equal (scale (newTone A Natural) Major)
                        [ newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        ]
            , test "A# Major" <|
                \() ->
                    Expect.equal (scale (newTone A Sharp) Major)
                        [ newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G SharpSharp
                        ]
            , test "B Major" <|
                \() ->
                    Expect.equal (scale (newTone B Natural) Major)
                        [ newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        ]
            ]
        , describe "minor scale tests"
            [ test "C minor" <|
                \() ->
                    Expect.equal (scale (newTone C Natural) Minor)
                        [ newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        ]
            , test "C# minor" <|
                \() ->
                    Expect.equal (scale (newTone C Sharp) Minor)
                        [ newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        ]
            , test "D minor" <|
                \() ->
                    Expect.equal (scale (newTone D Natural) Minor)
                        [ newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        ]
            , test "D# minor" <|
                \() ->
                    Expect.equal (scale (newTone D Sharp) Minor)
                        [ newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        ]
            , test "E minor" <|
                \() ->
                    Expect.equal (scale (newTone E Natural) Minor)
                        [ newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        ]
            , test "E# minor" <|
                \() ->
                    Expect.equal (scale (newTone E Sharp) Minor)
                        [ newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        , newTone D Sharp
                        ]
            , test "F minor" <|
                \() ->
                    Expect.equal (scale (newTone F Natural) Minor)
                        [ newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Flat
                        , newTone E Flat
                        ]
            , test "F# minor" <|
                \() ->
                    Expect.equal (scale (newTone F Sharp) Minor)
                        [ newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        ]
            , test "G minor" <|
                \() ->
                    Expect.equal (scale (newTone G Natural) Minor)
                        [ newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        ]
            , test "G# minor" <|
                \() ->
                    Expect.equal (scale (newTone G Sharp) Minor)
                        [ newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        ]
            , test "A minor" <|
                \() ->
                    Expect.equal (scale (newTone A Natural) Minor)
                        [ newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        ]
            , test "A# minor" <|
                \() ->
                    Expect.equal (scale (newTone A Sharp) Minor)
                        [ newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        ]
            , test "B minor" <|
                \() ->
                    Expect.equal (scale (newTone B Natural) Minor)
                        [ newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        ]
            ]
        ]
