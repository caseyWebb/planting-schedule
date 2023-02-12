module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import Css
import Data.PlantingDates as PlantingDates exposing (PlantingDate(..), PlantingDates)
import Dict
import Effect
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Maybe.Extra as Maybe
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (Path)
import Route
import RouteBuilder exposing (StatefulRoute, StaticPayload)
import Shared
import String.Extra as String
import Time exposing (Posix)
import Time.Extra as Time
import Time.Extra2 as Time exposing (DateRange)
import Tuple.Extra2 as Tuple
import View exposing (View)


type alias Model =
    { today : Maybe Posix }


type Msg
    = GotCurrentTime Posix


type alias RouteParams =
    {}


type alias Data =
    { plantingDates : PlantingDates
    }


type alias ActionData =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithLocalState
            { view = view
            , init = init
            , update = update
            , subscriptions = subscriptions
            }


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


data : BackendTask FatalError Data
data =
    BackendTask.map Data
        PlantingDates.data


init : Maybe PageUrl -> Shared.Model -> StaticPayload Data ActionData RouteParams -> ( Model, Effect.Effect Msg )
init _ _ _ =
    ( { today = Nothing
      }
    , Effect.GetCurrentTime GotCurrentTime
    )


update : PageUrl -> Shared.Model -> StaticPayload Data ActionData RouteParams -> Msg -> Model -> ( Model, Effect.Effect Msg )
update _ _ _ msg model =
    case msg of
        GotCurrentTime time ->
            ( { model | today = Just time }
            , Effect.none
            )


subscriptions : Maybe PageUrl -> RouteParams -> Path -> Shared.Model -> Model -> Sub Msg
subscriptions _ _ _ _ _ =
    Sub.none


view :
    Maybe PageUrl
    -> Shared.Model
    -> Model
    -> StaticPayload Data ActionData RouteParams
    -> View (Pages.Msg.Msg Msg)
view _ _ model static =
    { title = "North Texas Planting Schedule"
    , body = viewBody model static
    }


green : Css.Color
green =
    Css.hex "4E9F3D"


viewBody : Model -> StaticPayload Data ActionData RouteParams -> List (Html (Pages.Msg.Msg Msg))
viewBody model static =
    [ viewLegend
    , Html.div
        [ Attributes.css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "auto repeat(365, 1fr)"
            , Css.property "grid-template-rows" ("repeat(" ++ (Dict.size static.data.plantingDates |> String.fromInt) ++ ", 1fr)")
            , Css.borderRight3 (Css.px 3) Css.solid (Css.rgba 255 255 255 0.03)
            ]
        ]
        (viewMonthLabels
            |> Maybe.cons (Maybe.map viewTodayMarker model.today)
            |> List.append viewFrostDates
            |> List.append
                (Dict.toList static.data.plantingDates
                    |> List.indexedMap (\i -> viewPlant (i + 1))
                    |> List.concat
                )
        )
    ]


viewLegend : Html (Pages.Msg.Msg Msg)
viewLegend =
    Html.div
        [ Attributes.css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            ]
        ]
        [ Html.div
            [ Attributes.css
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.alignItems Css.center
                , Css.justifyContent Css.spaceBetween
                , Css.marginBottom (Css.px 100)
                , Css.property "gap" "5em"
                , Css.width (Css.pct 100)
                , Css.maxWidth (Css.px 800)
                ]
            ]
            [ Html.span
                [ Attributes.css
                    [ Css.borderBottom3 (Css.px 3) Css.solid green
                    , Css.paddingBottom (Css.px 10)
                    , Css.flex (Css.int 1)
                    ]
                ]
                [ Html.text "Direct sow" ]
            , Html.span
                [ Attributes.css
                    [ Css.borderBottom3 (Css.px 3) Css.dashed green
                    , Css.paddingBottom (Css.px 10)
                    , Css.flex (Css.int 1)
                    ]
                ]
                [ Html.text "Indirect sow" ]
            , Html.span
                [ Attributes.css
                    [ Css.borderBottom3 (Css.px 8) Css.double green
                    , Css.paddingBottom (Css.px 8)
                    , Css.flex (Css.int 1)
                    ]
                ]
                [ Html.text "Transplant" ]
            ]
        ]


viewMonthLabels : List (Html (Pages.Msg.Msg Msg))
viewMonthLabels =
    Time.months
        |> List.indexedMap
            (\i month ->
                viewDateRangeOverlay
                    (if modBy 2 i == 0 then
                        Css.rgba 255 255 255 0.03

                     else
                        Css.rgba 0 0 0 0
                    )
                    [ Css.fontSize (Css.px 18)
                    , Css.fontWeight Css.bold
                    , Css.textAlign Css.center
                    ]
                    [ Html.span
                        [ Attributes.css [ Css.position Css.relative, Css.top (Css.px -40) ]
                        ]
                        [ Html.text (Time.monthNameShort month) ]
                    ]
                    ( Time.firstDayOfMonth month, Time.lastDayOfMonth month )
            )


viewTodayMarker : Posix -> Html (Pages.Msg.Msg Msg)
viewTodayMarker today =
    viewDateRangeOverlay (Css.hex "EEBB4D") [] [] ( today, today )


viewFrostDates : List (Html (Pages.Msg.Msg Msg))
viewFrostDates =
    let
        lastFrost =
            ( Time.firstDayOfYear
            , Time.fromDateTuple Time.utc ( 1970, Time.Apr, 1 )
            )

        firstFrost =
            ( Time.fromDateTuple Time.utc ( 1970, Time.Nov, 1 )
            , Time.lastDayOfYear
            )
    in
    List.map (viewDateRangeOverlay (Css.hex "00337C") [] []) [ lastFrost, firstFrost ]


viewDateRangeOverlay : Css.Color -> List Css.Style -> List (Html msg) -> DateRange -> Html msg
viewDateRangeOverlay color styles contents span =
    let
        ( start, end ) =
            Tuple.mapSame (Time.toDayOfYear >> (+) 1 >> String.fromInt) span
    in
    Html.div
        [ Attributes.css
            [ Css.property "grid-column" (String.join "/" [ start, end ])
            , Css.property "grid-row" "1/-1"
            , Css.backgroundColor color
            , Css.zIndex (Css.int 0)
            , Css.batch styles
            ]
        ]
        contents


viewPlant : Int -> ( String, List PlantingDate ) -> List (Html (Pages.Msg.Msg Msg))
viewPlant row ( plant, dates ) =
    Html.h2
        [ Attributes.css
            [ Css.property "grid-column" "1"
            , Css.property "grid-row" (String.fromInt row)
            , Css.fontSize (Css.px 20)
            , Css.textAlign Css.right
            , Css.paddingRight (Css.em 1.2)
            ]
        ]
        [ Html.text (String.toTitleCase plant) ]
        :: List.concatMap (viewPlantingDate row) dates


viewPlantingDate : Int -> PlantingDate -> List (Html (Pages.Msg.Msg Msg))
viewPlantingDate row date =
    let
        timelines =
            case date of
                DirectSow span ->
                    [ ( Css.borderTop3 (Css.px 3) Css.solid green, span ) ]

                Transplant span sowWeeksPrior ->
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
                    ( Css.batch
                        [ Css.borderTop3 (Css.px 6) Css.double green
                        ]
                    , span
                    )
                        :: List.map
                            (Tuple.pair
                                (Css.batch
                                    [ Css.borderTop3 (Css.px 3) Css.dashed green
                                    ]
                                )
                            )
                            transplantTimelines
    in
    List.map (viewTimeline row) timelines


viewTimeline : Int -> ( Css.Style, DateRange ) -> Html (Pages.Msg.Msg Msg)
viewTimeline row ( styles, ( start, end ) ) =
    Html.div
        [ Attributes.css
            [ Css.height (Css.pct 100)
            , Css.zIndex (Css.int 1)
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
                [ styles, Css.width (Css.pct 100) ]
            ]
            []
        ]
