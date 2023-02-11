module Maybe.Extra2 exposing (..)


mapTuple : (a -> b -> c) -> ( Maybe a, Maybe b ) -> Maybe c
mapTuple fn ( a, b ) =
    Maybe.map2 fn a b
