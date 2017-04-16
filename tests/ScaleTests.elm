module ScaleTests exposing (all)

import Maestro.Note exposing (Note)
import Maestro.Tone exposing (Key(..), Adjustment(..), newTone)
import Maestro.Interval exposing (Interval(..), Degree(..))
import Maestro.Scale exposing (Scale, Mode(..), scale)
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Ionian scale tests"
            [ test "C Ionian" <|
                \() ->
                    Expect.equal (scale (newTone C Natural) Ionian)
                        [ newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        ]
            , test "C# Ionian" <|
                \() ->
                    Expect.equal (scale (newTone C Sharp) Ionian)
                        [ newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        ]
            , test "D Ionian" <|
                \() ->
                    Expect.equal (scale (newTone D Natural) Ionian)
                        [ newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        ]
            , test "D# Ionian" <|
                \() ->
                    Expect.equal (scale (newTone D Sharp) Ionian)
                        [ newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        ]
            , test "E Ionian" <|
                \() ->
                    Expect.equal (scale (newTone E Natural) Ionian)
                        [ newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        ]
            , test "Fb Ionian" <|
                \() ->
                    Expect.equal (scale (newTone F Flat) Ionian)
                        [ newTone F Flat
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        ]
            , test "F Ionian" <|
                \() ->
                    Expect.equal (scale (newTone F Natural) Ionian)
                        [ newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        ]
            , test "F# Ionian" <|
                \() ->
                    Expect.equal (scale (newTone F Sharp) Ionian)
                        [ newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        ]
            , test "G Ionian" <|
                \() ->
                    Expect.equal (scale (newTone G Natural) Ionian)
                        [ newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        ]
            , test "G# Ionian" <|
                \() ->
                    Expect.equal (scale (newTone G Sharp) Ionian)
                        [ newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        ]
            , test "A Ionian" <|
                \() ->
                    Expect.equal (scale (newTone A Natural) Ionian)
                        [ newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        ]
            , test "A# Ionian" <|
                \() ->
                    Expect.equal (scale (newTone A Sharp) Ionian)
                        [ newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G SharpSharp
                        ]
            , test "B Ionian" <|
                \() ->
                    Expect.equal (scale (newTone B Natural) Ionian)
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
                    Expect.equal (scale (newTone C Natural) Aeolian)
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
                    Expect.equal (scale (newTone C Sharp) Aeolian)
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
                    Expect.equal (scale (newTone D Natural) Aeolian)
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
                    Expect.equal (scale (newTone D Sharp) Aeolian)
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
                    Expect.equal (scale (newTone E Natural) Aeolian)
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
                    Expect.equal (scale (newTone E Sharp) Aeolian)
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
                    Expect.equal (scale (newTone F Natural) Aeolian)
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
                    Expect.equal (scale (newTone F Sharp) Aeolian)
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
                    Expect.equal (scale (newTone G Natural) Aeolian)
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
                    Expect.equal (scale (newTone G Sharp) Aeolian)
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
                    Expect.equal (scale (newTone A Natural) Aeolian)
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
                    Expect.equal (scale (newTone A Sharp) Aeolian)
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
                    Expect.equal (scale (newTone B Natural) Aeolian)
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
