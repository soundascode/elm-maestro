module Maestro.ListUtils exposing
    ( first
    , rotate
    )

import List


first : List a -> Maybe a
first l =
    case l of
        e :: _ ->
            Just e

        [] ->
            Nothing



{- Rotate a list n times -}


rotate : Int -> List a -> List a
rotate n l =
    List.drop n l ++ List.take n l
