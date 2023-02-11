module Tuple.Extra2 exposing (..)


mapSame : (a -> b) -> ( a, a ) -> ( b, b )
mapSame f =
    Tuple.mapBoth f f
