module Scale.Mixolydian exposing (all)

import Expect
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Note exposing (Note)
import Maestro.Scale exposing (Mode(..), Scale, scale)
import Maestro.Tone exposing (Adjustment(..), Key(..), newTone)
import Test exposing (..)


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Mixolydian scale tests"
            [ test "C Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone C Natural) Mixolydian)
                        [ newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        ]
            , test "C# Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone C Sharp) Mixolydian)
                        [ newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        ]
            , test "Db Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone D Flat) Mixolydian)
                        [ newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Flat
                        ]
            , test "D Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone D Natural) Mixolydian)
                        [ newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        ]
            , test "D# Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone D Sharp) Mixolydian)
                        [ newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        ]
            , test "Eb Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone E Flat) Mixolydian)
                        [ newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Flat
                        ]
            , test "E Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone E Natural) Mixolydian)
                        [ newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        ]
            , test "E# Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone E Sharp) Mixolydian)
                        [ newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G SharpSharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D Sharp
                        ]
            , test "Fb Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone F Flat) Mixolydian)
                        [ newTone F Flat
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E FlatFlat
                        ]
            , test "F Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone F Natural) Mixolydian)
                        [ newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        ]
            , test "F# Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone F Sharp) Mixolydian)
                        [ newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        ]
            , test "Gb Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone G Flat) Mixolydian)
                        [ newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Flat
                        ]
            , test "G Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone G Natural) Mixolydian)
                        [ newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        ]
            , test "G# Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone G Sharp) Mixolydian)
                        [ newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        ]
            , test "Ab Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone A Flat) Mixolydian)
                        [ newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Flat
                        ]
            , test "A Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone A Natural) Mixolydian)
                        [ newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        ]
            , test "A# Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone A Sharp) Mixolydian)
                        [ newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G Sharp
                        ]
            , test "Bb Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone B Flat) Mixolydian)
                        [ newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        ]
            , test "B Mixolydian" <|
                \() ->
                    Expect.equal (scale (newTone B Natural) Mixolydian)
                        [ newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        ]
            ]
        ]
