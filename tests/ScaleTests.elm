module ScaleTests exposing (all)

import Scale.AeolianTests
import Scale.IonianTests
import Scale.DorianTests
import Scale.PhrygianTests
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
        [ Scale.IonianTests.all
        , Scale.DorianTests.all
        , Scale.PhrygianTests.all
        , Scale.AeolianTests.all
        ]
