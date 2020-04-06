module Scale.Ionian exposing (all)

import Expect
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Note exposing (Note)
import Maestro.Scale exposing (Mode(..), Scale, scale)
import Maestro.Tone exposing (Adjustment(..), Key(..), newTone)
import Test exposing (..)


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
            , test "Db Ionian" <|
                \() ->
                    Expect.equal (scale (newTone D Flat) Ionian)
                        [ newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
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
            , test "Eb Ionian" <|
                \() ->
                    Expect.equal (scale (newTone E Flat) Ionian)
                        [ newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
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
            , test "E# Ionian" <|
                \() ->
                    Expect.equal (scale (newTone E Sharp) Ionian)
                        [ newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G SharpSharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D SharpSharp
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
            , test "Gb Ionian" <|
                \() ->
                    Expect.equal (scale (newTone G Flat) Ionian)
                        [ newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
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
            , test "Ab Ionian" <|
                \() ->
                    Expect.equal (scale (newTone A Flat) Ionian)
                        [ newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
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
            , test "Bb Ionian" <|
                \() ->
                    Expect.equal (scale (newTone B Flat) Ionian)
                        [ newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
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
        ]
