module Data.PlantingDates exposing (PlantingDate(..), PlantingDates, data)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Maybe.Extra2 as Maybe
import String
import String.Extra2 as String
import Time exposing (Posix)
import Time.Extra as Time


type alias PlantingDates =
    Dict String (List PlantingDate)


type PlantingDate
    = DirectSow ( Posix, Posix )
    | Transplant Int ( Posix, Posix )


data : BackendTask FatalError PlantingDates
data =
    File.jsonFile decode "data/planting-dates.json" |> BackendTask.mapError .fatal


decode : Decoder PlantingDates
decode =
    Decode.dict (Decode.list decodePlantingDate)


decodePlantingDate : Decoder PlantingDate
decodePlantingDate =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "direct-sow" ->
                        decodeDirectSow

                    "transplant" ->
                        decodeTransplant

                    _ ->
                        Decode.fail ("Invalid planting date type: " ++ type_)
            )


decodeDirectSow : Decoder PlantingDate
decodeDirectSow =
    Decode.map2 (\start end -> DirectSow ( start, end ))
        (Decode.field "start" decodePlantingTime)
        (Decode.field "end" decodePlantingTime)


decodeTransplant : Decoder PlantingDate
decodeTransplant =
    Decode.map3 (\weeks start end -> Transplant weeks ( start, end ))
        (Decode.field "sowWeeksPrior" Decode.int)
        (Decode.field "start" decodePlantingTime)
        (Decode.field "end" decodePlantingTime)


decodePlantingTime : Decoder Posix
decodePlantingTime =
    Decode.string
        |> Decode.andThen
            (String.splitTuple '/'
                >> Tuple.mapBoth (String.toInt >> Maybe.andThen Time.intToMonth) String.toInt
                >> Maybe.mapTuple (\m d -> Time.fromDateTuple Time.utc ( 1970, m, d ))
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Invalid date")
            )
