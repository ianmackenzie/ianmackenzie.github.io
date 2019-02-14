module Guide exposing (Config, app)

{-|

@docs Config, app

-}

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Element exposing (Element)
import Guide.Widget as Widget exposing (Widget)
import Html exposing (Html)
import Url exposing (Url)


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url


type alias Program =
    Platform.Program Flags Model Msg


type alias Config =
    { title : String
    , widgets : List ( String, Widget )
    }


initWith : Config -> Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
initWith config flags url navigationKey =
    ( (), Cmd.none )


viewWith : Config -> Model -> Browser.Document Msg
viewWith config model =
    { title = config.title
    , body =
        [ Html.text "Hello" ]
    }


updateWith : Config -> Msg -> Model -> ( Model, Cmd Msg )
updateWith config message model =
    ( model, Cmd.none )


subscriptionsWith : Config -> Model -> Sub Msg
subscriptionsWith config model =
    Sub.none


app : Config -> Program
app config =
    Browser.application
        { init = initWith config
        , view = viewWith config
        , update = updateWith config
        , subscriptions = subscriptionsWith config
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
