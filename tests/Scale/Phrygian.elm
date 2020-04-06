module Scale.Phrygian exposing (all)

import Expect
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Note exposing (Note)
import Maestro.Scale exposing (Mode(..), Scale, scale)
import Maestro.Tone exposing (Adjustment(..), Key(..), newTone)
import Test exposing (..)


all : Test
all =
    describe "Phrygian test suite"
        [ describe "phrygian mode tests"
            [ test "C Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone C Natural) Phrygian)
                        [ newTone C Natural
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        ]
            , test "C# Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone C Sharp) Phrygian)
                        [ newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        ]
            , test "Db Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone D Flat) Phrygian)
                        [ newTone D Flat
                        , newTone E FlatFlat
                        , newTone F Flat
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B FlatFlat
                        , newTone C Flat
                        ]
            , test "D Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone D Natural) Phrygian)
                        [ newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        ]
            , test "D# Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone D Sharp) Phrygian)
                        [ newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        ]
            , test "Eb Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone E Flat) Phrygian)
                        [ newTone E Flat
                        , newTone F Flat
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Flat
                        , newTone D Flat
                        ]
            , test "E Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone E Natural) Phrygian)
                        [ newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        ]
            , test "E# Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone E Sharp) Phrygian)
                        [ newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        , newTone D Sharp
                        ]
            , test "Fb Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone F Flat) Phrygian)
                        [ newTone F Flat
                        , newTone G FlatFlat
                        , newTone A FlatFlat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D FlatFlat
                        , newTone E FlatFlat
                        ]
            , test "F Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone F Natural) Phrygian)
                        [ newTone F Natural
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Flat
                        , newTone E Flat
                        ]
            , test "F# Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone F Sharp) Phrygian)
                        [ newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        ]
            , test "Gb Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone G Flat) Phrygian)
                        [ newTone G Flat
                        , newTone A FlatFlat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E FlatFlat
                        , newTone F Flat
                        ]
            , test "G Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone G Natural) Phrygian)
                        [ newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        ]
            , test "G# Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone G Sharp) Phrygian)
                        [ newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        ]
            , test "Ab Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone A Flat) Phrygian)
                        [ newTone A Flat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Flat
                        , newTone G Flat
                        ]
            , test "A Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone A Natural) Phrygian)
                        [ newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        ]
            , test "A# Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone A Sharp) Phrygian)
                        [ newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        ]
            , test "Bb Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone B Flat) Phrygian)
                        [ newTone B Flat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Flat
                        , newTone A Flat
                        ]
            , test "B Phrygian" <|
                \() ->
                    Expect.equal (scale (newTone B Natural) Phrygian)
                        [ newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        ]
            ]
        ]
