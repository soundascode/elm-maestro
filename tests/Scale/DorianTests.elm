module Scale.DorianTests exposing (all)

import Maestro.Note exposing (Note)
import Maestro.Tone exposing (Key(..), Adjustment(..), newTone)
import Maestro.Interval exposing (Interval(..), Degree(..))
import Maestro.Scale exposing (Scale, Mode(..), scale)
import Test exposing (..)
import Expect


all : Test
all =
    describe "Dorian test suite"
        [ describe "dorian mode tests"
            [ test "C Dorian" <|
                \() ->
                    Expect.equal (scale (newTone C Natural) Dorian)
                        [ newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        ]
            , test "C# Dorian" <|
                \() ->
                    Expect.equal (scale (newTone C Sharp) Dorian)
                        [ newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        ]
            , test "Db Dorian" <|
                \() ->
                    Expect.equal (scale (newTone D Flat) Dorian)
                        [ newTone D Flat
                        , newTone E Flat
                        , newTone F Flat
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Flat
                        ]
            , test "D Dorian" <|
                \() ->
                    Expect.equal (scale (newTone D Natural) Dorian)
                        [ newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        ]
            , test "D# Dorian" <|
                \() ->
                    Expect.equal (scale (newTone D Sharp) Dorian)
                        [ newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        ]
            , test "Eb Dorian" <|
                \() ->
                    Expect.equal (scale (newTone E Flat) Dorian)
                        [ newTone E Flat
                        , newTone F Natural
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Flat
                        ]
            , test "E Dorian" <|
                \() ->
                    Expect.equal (scale (newTone E Natural) Dorian)
                        [ newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        ]
            , test "E# Dorian" <|
                \() ->
                    Expect.equal (scale (newTone E Sharp) Dorian)
                        [ newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D Sharp
                        ]
            , test "Fb Dorian" <|
                \() ->
                    Expect.equal (scale (newTone F Flat) Dorian)
                        [ newTone F Flat
                        , newTone G Flat
                        , newTone A FlatFlat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E FlatFlat
                        ]
            , test "F Dorian" <|
                \() ->
                    Expect.equal (scale (newTone F Natural) Dorian)
                        [ newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        ]
            , test "F# Dorian" <|
                \() ->
                    Expect.equal (scale (newTone F Sharp) Dorian)
                        [ newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        ]
            , test "Gb Dorian" <|
                \() ->
                    Expect.equal (scale (newTone G Flat) Dorian)
                        [ newTone G Flat
                        , newTone A Flat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Flat
                        ]
            , test "G Dorian" <|
                \() ->
                    Expect.equal (scale (newTone G Natural) Dorian)
                        [ newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        ]
            , test "G# Dorian" <|
                \() ->
                    Expect.equal (scale (newTone G Sharp) Dorian)
                        [ newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        ]
            , test "Ab Dorian" <|
                \() ->
                    Expect.equal (scale (newTone A Flat) Dorian)
                        [ newTone A Flat
                        , newTone B Flat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Flat
                        ]
            , test "A Dorian" <|
                \() ->
                    Expect.equal (scale (newTone A Natural) Dorian)
                        [ newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        ]
            , test "A# Dorian" <|
                \() ->
                    Expect.equal (scale (newTone A Sharp) Dorian)
                        [ newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G Sharp
                        ]
            , test "Bb Dorian" <|
                \() ->
                    Expect.equal (scale (newTone B Flat) Dorian)
                        [ newTone B Flat
                        , newTone C Natural
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        ]
            , test "B Dorian" <|
                \() ->
                    Expect.equal (scale (newTone B Natural) Dorian)
                        [ newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        ]
            , test "B# Dorian" <|
                \() ->
                    Expect.equal (scale (newTone B Sharp) Dorian)
                        [ newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G SharpSharp
                        , newTone A Sharp
                        ]
            ]
        ]
