module Maestro.Function exposing (Function(..), toDegree, toRomanNumeral, toString)

import Maestro.Degree exposing (Degree(..))
import Maestro.Quality exposing (Quality(..))


type Function
    = Tonic
    | Supertonic
    | Mediant
    | Subdominant
    | Dominant
    | Submediant
    | LeadingTone


quality : Function -> Quality
quality f =
    case f of
        Tonic ->
            Major

        Supertonic ->
            Minor

        Mediant ->
            Minor

        Subdominant ->
            Major

        Dominant ->
            Major

        Submediant ->
            Minor

        LeadingTone ->
            Minor


toDegree : Function -> Degree
toDegree f =
    case f of
        Tonic ->
            Root

        Supertonic ->
            Second

        Mediant ->
            Third

        Subdominant ->
            Fourth

        Dominant ->
            Fifth

        Submediant ->
            Sixth

        LeadingTone ->
            Seventh


toRomanNumeral : Function -> String
toRomanNumeral f =
    case f of
        Tonic ->
            "I"

        Supertonic ->
            "ii"

        Mediant ->
            "iii"

        Subdominant ->
            "IV"

        Dominant ->
            "V"

        Submediant ->
            "vi"

        LeadingTone ->
            "vii"


toString : Function -> String
toString f =
    case f of
        Tonic ->
            "tonic"

        Supertonic ->
            "supertonic"

        Mediant ->
            "mediant"

        Subdominant ->
            "subdominant"

        Dominant ->
            "dominant"

        Submediant ->
            "submediant"

        LeadingTone ->
            "leading tone"
