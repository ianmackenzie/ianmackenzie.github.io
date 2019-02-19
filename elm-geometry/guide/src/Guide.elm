module Guide exposing (Program, program)

{-|

@docs Config, app

-}

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Dict
import Element exposing (Element)
import Guide.Document as Document exposing (Document)
import Guide.Page as Page exposing (Page)
import Guide.Screen as Screen exposing (Screen)
import Guide.Widget as Widget exposing (Widget)
import Html exposing (Html)
import Http
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>))
import Url.Parser.Query


type alias Flags =
    { width : Int
    , height : Int
    }


stripWidth : Int
stripWidth =
    10


navWidth : Int
navWidth =
    300


documentAvailableWidth : Screen -> Int
documentAvailableWidth screen =
    case screen.class of
        Screen.Small ->
            screen.width - stripWidth

        Screen.Large ->
            screen.width - navWidth


handleMarkdown : Screen -> Page -> Result Http.Error String -> Msg
handleMarkdown screen page result =
    case result of
        Ok markdown ->
            let
                documentConfig =
                    { availableWidth = documentAvailableWidth screen
                    , screenClass = screen.class
                    , widgets = []
                    }
            in
            case Document.parse documentConfig markdown of
                Ok document ->
                    DocumentLoaded page document

                Err message ->
                    LoadError message

        Err error ->
            LoadError (Debug.toString error)


loadPage : Screen -> Page -> Cmd Msg
loadPage screen page =
    Http.get
        { url = Page.sourceUrl page
        , expect = Http.expectString (handleMarkdown screen page)
        }


type State
    = Loading Page
    | Error String
    | Loaded Page Document


title : State -> String
title state =
    case state of
        Loading page ->
            Page.title page

        Error _ ->
            "Error!"

        Loaded page _ ->
            Page.title page


type alias Model =
    { screen : Screen
    , navigationKey : Navigation.Key
    , pages : List Page
    , state : State
    }


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | LoadError String
    | DocumentLoaded Page Document
    | DocumentUpdated Document


type alias Program =
    Platform.Program Flags Model Msg


handleNewUrl : List Page -> Screen -> Url -> ( State, Cmd Msg )
handleNewUrl pages screen url =
    case Page.matching url pages of
        Just { page, fragment } ->
            ( Loading page, loadPage screen page )

        Nothing ->
            let
                errorMessage =
                    "No page matching " ++ Url.toString url
            in
            ( Error errorMessage, Cmd.none )


init : Page -> List Page -> Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init readmePage allPages flags url navigationKey =
    let
        screen =
            Screen.init { width = flags.width }

        ( state, command ) =
            handleNewUrl allPages screen url
    in
    ( { pages = allPages
      , state = state
      , screen = screen
      , navigationKey = navigationKey
      }
    , command
    )


view : Model -> Browser.Document Msg
view model =
    { title = title model.state
    , body =
        case model.state of
            Loading _ ->
                [ Html.text "Loading..." ]

            Loaded _ document ->
                [ Element.layout [ Element.width Element.fill ]
                    (Document.view [ Element.centerX ] document |> Element.map DocumentUpdated)
                ]

            Error message ->
                [ Html.text message ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( model, Navigation.load url )

        UrlChanged url ->
            let
                ( state, command ) =
                    handleNewUrl model.pages model.screen url
            in
            ( { model | state = state }, command )

        LoadError string ->
            ( { model | state = Error string }, Cmd.none )

        DocumentLoaded documentPage document ->
            case model.state of
                Loading loadingPage ->
                    if documentPage == loadingPage then
                        ( { model | state = Loaded documentPage document }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DocumentUpdated newDocument ->
            case model.state of
                Loaded page currentDocument ->
                    ( { model | state = Loaded page newDocument }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


program :
    { readmeUrl : String
    , pages : List { title : String, widgets : List ( String, Widget ) }
    }
    -> Program
program { readmeUrl, pages } =
    let
        readmePage =
            Page.readme { url = readmeUrl }

        allPages =
            readmePage :: List.map Page.with pages
    in
    Browser.application
        { init = init readmePage allPages
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
