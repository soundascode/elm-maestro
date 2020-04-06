module Scale.Lydian exposing (all)

import Expect
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Note exposing (Note)
import Maestro.Scale exposing (Mode(..), Scale, scale)
import Maestro.Tone exposing (Adjustment(..), Key(..), newTone)
import Test exposing (..)


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Lydian scale tests"
            [ test "C Lydian" <|
                \() ->
                    Expect.equal (scale (newTone C Natural) Lydian)
                        [ newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        ]
            , test "C# Lydian" <|
                \() ->
                    Expect.equal (scale (newTone C Sharp) Lydian)
                        [ newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        ]
            , test "Db Lydian" <|
                \() ->
                    Expect.equal (scale (newTone D Flat) Lydian)
                        [ newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        ]
            , test "D Lydian" <|
                \() ->
                    Expect.equal (scale (newTone D Natural) Lydian)
                        [ newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        ]
            , test "D# Lydian" <|
                \() ->
                    Expect.equal (scale (newTone D Sharp) Lydian)
                        [ newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G SharpSharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        ]
            , test "Eb Lydian" <|
                \() ->
                    Expect.equal (scale (newTone E Flat) Lydian)
                        [ newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        ]
            , test "E Lydian" <|
                \() ->
                    Expect.equal (scale (newTone E Natural) Lydian)
                        [ newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        ]
            , test "E# Lydian" <|
                \() ->
                    Expect.equal (scale (newTone E Sharp) Lydian)
                        [ newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G SharpSharp
                        , newTone A SharpSharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D SharpSharp
                        ]
            , test "Fb Lydian" <|
                \() ->
                    Expect.equal (scale (newTone F Flat) Lydian)
                        [ newTone F Flat
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        ]
            , test "F Lydian" <|
                \() ->
                    Expect.equal (scale (newTone F Natural) Lydian)
                        [ newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        ]
            , test "F# Lydian" <|
                \() ->
                    Expect.equal (scale (newTone F Sharp) Lydian)
                        [ newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        ]
            , test "Gb Lydian" <|
                \() ->
                    Expect.equal (scale (newTone G Flat) Lydian)
                        [ newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        ]
            , test "G Lydian" <|
                \() ->
                    Expect.equal (scale (newTone G Natural) Lydian)
                        [ newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        ]
            , test "G# Lydian" <|
                \() ->
                    Expect.equal (scale (newTone G Sharp) Lydian)
                        [ newTone G Sharp
                        , newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        ]
            , test "Ab Lydian" <|
                \() ->
                    Expect.equal (scale (newTone A Flat) Lydian)
                        [ newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        ]
            , test "A Lydian" <|
                \() ->
                    Expect.equal (scale (newTone A Natural) Lydian)
                        [ newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        ]
            , test "A# Lydian" <|
                \() ->
                    Expect.equal (scale (newTone A Sharp) Lydian)
                        [ newTone A Sharp
                        , newTone B Sharp
                        , newTone C SharpSharp
                        , newTone D SharpSharp
                        , newTone E Sharp
                        , newTone F SharpSharp
                        , newTone G SharpSharp
                        ]
            , test "Bb Lydian" <|
                \() ->
                    Expect.equal (scale (newTone B Flat) Lydian)
                        [ newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        ]
            , test "B Lydian" <|
                \() ->
                    Expect.equal (scale (newTone B Natural) Lydian)
                        [ newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        ]
            ]
        ]
