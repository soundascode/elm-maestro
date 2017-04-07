# elm-maestro

This library provides music theory abstractions and functionalities.

**Alpha stage**: *this library is not considered stable yet, and could be
subject to API changes in the future.*

## Installation

```bash
elm-package oleiade/elm-maestro
```

or add the dependency in your `elm-package.json` file directly

```json
"oleiade/elm-maestro": "0.1.0 <= v < 0.2.0"
```

## Usage

### Types and abstractions

#### Tone

The `Maestro.Tone` module exposes the following types:

```elm
type alias Tone =
    { key : Key, adjustment : Adjustment }

type Key
    = C
    | D
    | E
    | F
    | G
    | A
    | B

type Adjustment
    = Natural
    | Sharp
    | Flat
    | SharpSharp
    | FlatFlat
```

in order to represent pitches/tones as follows:

```elm
import Maestro.Tone exposing(Key(..), Adjustment(..))

cNatural = newTone C Natural
dSharp = newTone D Sharp
bFlat = newTone B Flat
```

#### Note

The `Maestro.Note` module exposes the following types:

```elm
type alias Note =
    { tone : Tone
    , octave : Octave
    }

type alias Octave =
    Int
```

in order to represent notes as follows:

```elm
import Maestro.Tone exposing (Tone, Key(..), Adjustment(..))
import Maestro.Note exposing (newNote)

e3Natural = newNote E Natural 3
f4Sharp = newNote F Sharp 4
a1Flat = newNote A Flat 1
```

#### Interval

The `Maestro.Interval` module exposes the following type:

```elm
type Interval
    = Unison
    | MinorSecond
    | MajorSecond
    | MinorThird
    | MajorThird
    | PerfectFourth
    | PerfectFifth
    | MinorSixth
    | MajorSixth
    | MinorSeventh
    | MajorSeventh
    | PerfectOctave
    | MinorNinth
    | MajorNinth
    | MinorTenth
    | MajorTenth
    | PerfectEleventh
    | AugmentedEleventh
    | PerfectTwelfth
    | MinorThirteen
    | MajorThirteen
    | MinorFourteenth
    | MajorFourteenth
    | DoubleOctave
```

and will allow you to find the note at interval from a start note
like follows:

```elm
import Maestro.Tone exposing (Key(..), Adjustment(..), newTone)
import Maestro.Interval exposing (Interval(..), addInterval)

example : Note
example =
  addInterval (newNote C Natural 3)  MajorThird
```

### Scale

A Scale is represented as a list of Tone. It is built from a pitch and a mode as
follows:

```elm
import Maestro.Tone exposing (newTone, Key(..), Adjustment(..))
import Maestro.Scale exposing (scale, Mode(..))

example : Scale
example =
    doSomethingWith <| scale (newTone C Natural) Major

```
