module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import BackendTask exposing (BackendTask)
import Css
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Html
import Html.Styled
import Html.Styled.Attributes as Attributes
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Nothing
    }


type Msg
    = SharedMsg SharedMsg


type alias Data =
    ()


type SharedMsg
    = NoOp


type alias Model =
    {}


init :
    Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init _ _ =
    ( {}
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SharedMsg _ ->
            ( model, Effect.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : BackendTask FatalError Data
data =
    BackendTask.succeed ()


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : List (Html.Html msg), title : String }
view _ _ _ _ pageView =
    { body =
        [ Html.Styled.toUnstyled <|
            Html.Styled.main_
                [ Attributes.css
                    [ Css.padding2 (Css.px 100) (Css.px 50)
                    , Css.minHeight (Css.calc (Css.vh 100) Css.minus (Css.px 100))
                    , Css.displayFlex
                    , Css.justifyContent Css.center
                    , Css.position Css.relative
                    ]
                ]
                [ Html.Styled.div
                    [ Attributes.css
                        [ Css.maxWidth (Css.px 1200)
                        , Css.flexGrow (Css.int 1)
                        ]
                    ]
                    pageView.body
                , viewFooter
                ]
        ]
    , title = pageView.title
    }


viewFooter : Html.Styled.Html msg
viewFooter =
    Html.Styled.footer
        [ Attributes.css
            [ Css.position Css.absolute
            , Css.bottom (Css.em 1.2)
            , Css.right (Css.em 1.2)
            , Css.fontWeight Css.bold
            , Css.fontSize (Css.px 12)
            ]
        ]
        [ Html.Styled.a
            [ Attributes.alt "View Source Code"
            , Attributes.href "https://github.com/caseyWebb/planting-schedule"
            ]
            [ Html.Styled.text "View Source Code" ]
        ]
