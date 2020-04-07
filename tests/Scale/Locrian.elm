module Scale.Locrian exposing (all)

import Expect
import Maestro.Interval exposing (Degree(..), Interval(..))
import Maestro.Note exposing (Note)
import Maestro.PitchClass exposing (Adjustment(..), PitchClass(..), newTone)
import Maestro.Scale exposing (Mode(..), Scale, scale)
import String
import Test exposing (..)


all : Test
all =
    describe "Scale Test Suite"
        [ describe "locrian scale tests"
            [ test "C locrian" <|
                \() ->
                    Expect.equal (scale (newTone C Natural) Locrian)
                        [ newTone C Natural
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        ]
            , test "C# locrian" <|
                \() ->
                    Expect.equal (scale (newTone C Sharp) Locrian)
                        [ newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        ]
            , test "Db locrian" <|
                \() ->
                    Expect.equal (scale (newTone D Flat) Locrian)
                        [ newTone D Flat
                        , newTone E FlatFlat
                        , newTone F Flat
                        , newTone G Flat
                        , newTone A FlatFlat
                        , newTone B FlatFlat
                        , newTone C Flat
                        ]
            , test "D locrian" <|
                \() ->
                    Expect.equal (scale (newTone D Natural) Locrian)
                        [ newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        ]
            , test "D# locrian" <|
                \() ->
                    Expect.equal (scale (newTone D Sharp) Locrian)
                        [ newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        ]
            , test "Eb locrian" <|
                \() ->
                    Expect.equal (scale (newTone E Flat) Locrian)
                        [ newTone E Flat
                        , newTone F Flat
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D Flat
                        ]
            , test "E locrian" <|
                \() ->
                    Expect.equal (scale (newTone E Natural) Locrian)
                        [ newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        ]
            , test "E# locrian" <|
                \() ->
                    Expect.equal (scale (newTone E Sharp) Locrian)
                        [ newTone E Sharp
                        , newTone F Sharp
                        , newTone G Sharp
                        , newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        ]
            , test "Fb locrian" <|
                \() ->
                    Expect.equal (scale (newTone F Flat) Locrian)
                        [ newTone F Flat
                        , newTone G FlatFlat
                        , newTone A FlatFlat
                        , newTone B FlatFlat
                        , newTone C FlatFlat
                        , newTone D FlatFlat
                        , newTone E FlatFlat
                        ]
            , test "F locrian" <|
                \() ->
                    Expect.equal (scale (newTone F Natural) Locrian)
                        [ newTone F Natural
                        , newTone G Flat
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        ]
            , test "F# locrian" <|
                \() ->
                    Expect.equal (scale (newTone F Sharp) Locrian)
                        [ newTone F Sharp
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        ]
            , test "Gb locrian" <|
                \() ->
                    Expect.equal (scale (newTone G Flat) Locrian)
                        [ newTone G Flat
                        , newTone A FlatFlat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D FlatFlat
                        , newTone E FlatFlat
                        , newTone F Flat
                        ]
            , test "G locrian" <|
                \() ->
                    Expect.equal (scale (newTone G Natural) Locrian)
                        [ newTone G Natural
                        , newTone A Flat
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Natural
                        ]
            , test "G# locrian" <|
                \() ->
                    Expect.equal (scale (newTone G Sharp) Locrian)
                        [ newTone G Sharp
                        , newTone A Natural
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Sharp
                        ]
            , test "Ab locrian" <|
                \() ->
                    Expect.equal (scale (newTone A Flat) Locrian)
                        [ newTone A Flat
                        , newTone B FlatFlat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E FlatFlat
                        , newTone F Flat
                        , newTone G Flat
                        ]
            , test "A locrian" <|
                \() ->
                    Expect.equal (scale (newTone A Natural) Locrian)
                        [ newTone A Natural
                        , newTone B Flat
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Flat
                        , newTone F Natural
                        , newTone G Natural
                        ]
            , test "A# locrian" <|
                \() ->
                    Expect.equal (scale (newTone A Sharp) Locrian)
                        [ newTone A Sharp
                        , newTone B Natural
                        , newTone C Sharp
                        , newTone D Sharp
                        , newTone E Natural
                        , newTone F Sharp
                        , newTone G Sharp
                        ]
            , test "Bb locrian" <|
                \() ->
                    Expect.equal (scale (newTone B Flat) Locrian)
                        [ newTone B Flat
                        , newTone C Flat
                        , newTone D Flat
                        , newTone E Flat
                        , newTone F Flat
                        , newTone G Flat
                        , newTone A Flat
                        ]
            , test "B locrian" <|
                \() ->
                    Expect.equal (scale (newTone B Natural) Locrian)
                        [ newTone B Natural
                        , newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        ]
            ]
        ]
