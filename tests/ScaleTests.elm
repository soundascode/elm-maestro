module ScaleTests exposing (all)

import Scale.AeolianTests
import Scale.IonianTests
import Scale.DorianTests
import Scale.PhrygianTests
import Scale.LydianTests
import Scale.MixolydianTests
import Scale.LocrianTests
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
        , Scale.LydianTests.all
        , Scale.MixolydianTests.all
        , Scale.AeolianTests.all
        , Scale.LocrianTests.all
        ]
