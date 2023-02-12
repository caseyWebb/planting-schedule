module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import Css
import Data.PlantingDates as PlantingDates exposing (PlantingDate(..), PlantingDates)
import Dict
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path
import Route
import RouteBuilder exposing (StatelessRoute, StaticPayload)
import Shared
import Time exposing (Posix)
import Time.Extra as Time
import Time.Extra2 as Time
import Tuple.Extra2 as Tuple
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { plantingDates : PlantingDates
    }


type alias ActionData =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


data : BackendTask FatalError Data
data =
    BackendTask.map Data
        PlantingDates.data


head :
    StaticPayload Data ActionData RouteParams
    -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = [ "images", "icon-png.png" ] |> Path.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Welcome to elm-pages!"
        , locale = Nothing
        , title = "elm-pages is running"
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data ActionData RouteParams
    -> View (Pages.Msg.Msg Msg)
view _ _ static =
    { title = "North Texas Planting Schedule"
    , body = viewBody static
    }


viewBody : StaticPayload Data ActionData RouteParams -> List (Html (Pages.Msg.Msg Msg))
viewBody static =
    [ Html.h1 [] [ Html.text "North Texas Planting Schedule" ]
    , Html.div
        [ Attributes.css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "150px repeat(365, 1fr)"
            , Css.property "grid-template-rows" ("repeat(" ++ (Dict.size static.data.plantingDates |> String.fromInt) ++ ", 1fr)")
            ]
        ]
        (viewHeader
            ++ (Dict.toList static.data.plantingDates
                    |> List.indexedMap (\i -> viewPlant (i + 1))
                    |> List.concat
               )
        )
    ]


viewHeader : List (Html (Pages.Msg.Msg Msg))
viewHeader =
    Time.months
        |> List.indexedMap
            (\i month ->
                let
                    ( start, end ) =
                        ( month, month )
                            |> Tuple.mapBoth Time.firstDayOfMonth Time.lastDayOfMonth
                            |> Tuple.mapSame (Time.toDayOfYear >> (+) 1 >> String.fromInt)
                in
                Html.div
                    [ Attributes.css
                        [ Css.property "grid-column" (String.join "/" [ start, end ])
                        , Css.property "grid-row" "1/-1"
                        , Css.textAlign Css.center
                        , Css.backgroundColor
                            (if modBy 2 i == 0 then
                                Css.hex "F0F0F0"

                             else
                                Css.hex "E0E0E0"
                            )
                        ]
                    ]
                    [ Html.text (Time.monthNameShort month) ]
            )


viewPlant : Int -> ( String, List PlantingDate ) -> List (Html (Pages.Msg.Msg Msg))
viewPlant row ( plant, dates ) =
    Html.h2
        [ Attributes.css
            [ Css.property "grid-column" "1"
            , Css.property "grid-row" (String.fromInt row)
            ]
        ]
        [ Html.text plant ]
        :: List.concatMap (viewPlantingDate row) dates


viewPlantingDate : Int -> PlantingDate -> List (Html (Pages.Msg.Msg Msg))
viewPlantingDate row date =
    let
        green =
            Css.hex "D7E9B9"

        yellow =
            Css.hex "FFFBAC"

        timelines =
            case date of
                DirectSow span ->
                    [ ( green, span ) ]

                Transplant sowWeeksPrior span ->
                    let
                        transplantSpan =
                            Tuple.mapSame (Time.subWeeks sowWeeksPrior) span

                        ( transplantStartDayOfYear, transplantEndDayOfYear ) =
                            Tuple.mapSame Time.toDayOfYear transplantSpan

                        transplantTimelines =
                            if transplantStartDayOfYear > transplantEndDayOfYear then
                                [ ( Time.firstDayOfYear, Tuple.second transplantSpan )
                                , ( Tuple.first transplantSpan, Time.lastDayOfYear )
                                ]

                            else
                                [ transplantSpan ]
                    in
                    ( green, span ) :: List.map (Tuple.pair yellow) transplantTimelines
    in
    List.map (viewTimeline row) timelines


viewTimeline : Int -> ( Css.Color, ( Posix, Posix ) ) -> Html (Pages.Msg.Msg Msg)
viewTimeline row ( color, ( start, end ) ) =
    Html.div
        [ Attributes.css
            [ Css.height (Css.pct 100)
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.property "grid-row" (String.fromInt row)
            , Css.property "grid-column"
                ([ start, end ]
                    |> List.map (Time.toDayOfYear >> (+) 1 >> String.fromInt)
                    |> String.join " / "
                )
            ]
        ]
        [ Html.div
            [ Attributes.css
                [ Css.backgroundColor color
                , Css.height (Css.px 20)
                , Css.width (Css.pct 100)
                ]
            ]
            []
        ]
