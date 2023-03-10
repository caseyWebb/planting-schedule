module Effect exposing (Effect(..), batch, fromCmd, map, none, perform)

{-|

@docs Effect, batch, fromCmd, map, none, perform

-}

import Browser.Navigation
import Form.FormData exposing (FormData)
import Http
import Pages.Fetcher
import Task
import Time exposing (Posix)
import Url exposing (Url)


{-| -}
type Effect msg
    = None
    | Cmd (Cmd msg)
    | Batch (List (Effect msg))
    | SubmitFetcher (Pages.Fetcher.Fetcher msg)
    | GetCurrentTime (Posix -> msg)


{-| -}
none : Effect msg
none =
    None


{-| -}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| -}
fromCmd : Cmd msg -> Effect msg
fromCmd =
    Cmd


{-| -}
map : (a -> b) -> Effect a -> Effect b
map fn effect =
    case effect of
        None ->
            None

        Cmd cmd ->
            Cmd (Cmd.map fn cmd)

        Batch list ->
            Batch (List.map (map fn) list)

        SubmitFetcher fetcher ->
            fetcher
                |> Pages.Fetcher.map fn
                |> SubmitFetcher

        GetCurrentTime toMsg ->
            GetCurrentTime (toMsg >> fn)


{-| -}
perform :
    { fetchRouteData :
        { data : Maybe FormData
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg
    , submit :
        { values : FormData
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg
    , runFetcher :
        Pages.Fetcher.Fetcher pageMsg
        -> Cmd msg
    , fromPageMsg : pageMsg -> msg
    , key : Browser.Navigation.Key
    , setField : { formId : String, name : String, value : String } -> Cmd msg
    }
    -> Effect pageMsg
    -> Cmd msg
perform ({ fromPageMsg, runFetcher } as helpers) effect =
    case effect of
        None ->
            Cmd.none

        Cmd cmd ->
            Cmd.map fromPageMsg cmd

        Batch list ->
            Cmd.batch (List.map (perform helpers) list)

        SubmitFetcher record ->
            runFetcher record

        GetCurrentTime toMsg ->
            Time.now |> Task.perform (toMsg >> fromPageMsg)
