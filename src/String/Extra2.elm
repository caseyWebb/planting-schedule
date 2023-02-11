module String.Extra2 exposing (..)

import List
import List.Extra as List


splitTuple : Char -> String -> ( String, String )
splitTuple c str =
    let
        chars =
            String.toList str
    in
    ( List.takeWhile ((/=) c) chars, List.takeWhileRight ((/=) c) chars )
        |> Tuple.mapBoth String.fromList String.fromList
