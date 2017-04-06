module IntervalTests exposing (..)

import Maestro.Pitch exposing (Key(..), Accidental(..))
import Maestro.Note exposing (Note)
import Maestro.Interval exposing (Interval(..), Degree(..))
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "Degree Test Suite"
        [ describe "substractDegree tests"
            []
          -- [ test "in current octave" <|
          --     \() ->
          --         Expect.equal (substractDegree Seventh 1) Sixth
          -- , test "substractDegree canceling operation" <|
          --     \() -> Expect.equal (substractDegree First 1) First
          -- , test "substractDegree underflow" <|
          --     \() -> Expect.equal (substractDegree First 2) First
          -- ]
        ]
