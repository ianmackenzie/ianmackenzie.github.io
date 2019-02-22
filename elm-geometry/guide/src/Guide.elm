module Guide exposing (Program, program)

{-|

@docs Config, app

-}

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Guide.Color as Color
import Guide.Document as Document exposing (Document)
import Guide.Font as Font
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
            LoadError "Network error"


loadPage : Screen -> List String -> Page -> Cmd Msg
loadPage screen rootPath page =
    Http.get
        { url = Page.sourceUrl rootPath page
        , expect = Http.expectString (handleMarkdown screen page)
        }


type State
    = Loading Page
    | Error String
    | Loaded Page Document


type alias Model =
    { screen : Screen
    , navigationKey : Navigation.Key
    , rootPath : List String
    , title : String
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


handleNewUrl : List String -> List Page -> Screen -> Url -> ( State, Cmd Msg )
handleNewUrl rootPath pages screen url =
    case Page.matching { url = url, rootPath = rootPath } pages of
        Just { page, fragment } ->
            ( Loading page, loadPage screen rootPath page )

        Nothing ->
            let
                errorMessage =
                    "No page matching " ++ Url.toString url
            in
            ( Error errorMessage, Cmd.none )


init : String -> Page -> List Page -> Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init title readmePage allPages flags url navigationKey =
    let
        screen =
            Screen.init { width = flags.width }

        rootPath =
            url.path |> String.split "/" |> List.filter (not << String.isEmpty)

        ( state, command ) =
            handleNewUrl rootPath allPages screen url
    in
    ( { title = title
      , pages = allPages
      , state = state
      , screen = screen
      , navigationKey = navigationKey
      , rootPath = rootPath
      }
    , command
    )


toPageLink : List String -> Page -> Page -> Element msg
toPageLink rootPath currentPage page =
    let
        attributes =
            if page == currentPage then
                [ Font.bold ]

            else
                []
    in
    Element.link attributes
        { url = Page.displayedUrl rootPath page Nothing
        , label = Element.text (Page.title page)
        }


navTitle : Model -> Element msg
navTitle model =
    Element.el
        [ Font.color Color.black
        , Font.bold
        , Font.size (Font.sizes model.screen.class).navTitle
        , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
        , Element.paddingEach { top = 0, bottom = 8, left = 0, right = 0 }
        , Border.color Color.dividerLine
        , Element.width Element.fill
        ]
        (Element.text model.title)


viewNav : Model -> Page -> Element msg
viewNav model currentPage =
    let
        navElement =
            Element.column
                [ Element.height Element.fill
                , Element.width Element.fill
                , Background.color Color.white
                , Border.widthEach { top = 0, bottom = 0, left = 0, right = 1 }
                , Border.color Color.navBorder
                , Font.alegreyaSans
                , Font.color Color.linkText
                , Font.size (Font.sizes model.screen.class).navText
                , Font.regular
                , Element.spacing 8
                , Element.padding 8
                ]
                (navTitle model :: List.map (toPageLink model.rootPath currentPage) model.pages)
    in
    Element.el
        [ Element.height Element.fill
        , Element.width (Element.px navWidth)
        , Element.clipY
        , Element.scrollbarY
        ]
        navElement


viewDocument : Model -> Page -> Document -> Element Msg
viewDocument model currentPage loadedDocument =
    let
        documentElement =
            Document.view [ Element.centerX ] loadedDocument
                |> Element.map DocumentUpdated
    in
    Element.el
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.clipY
        , Element.scrollbarY
        ]
        documentElement


view : Model -> Browser.Document Msg
view model =
    { title = model.title
    , body =
        [ Element.layout [ Element.width Element.fill, Element.height Element.fill ] <|
            case model.state of
                Loading page ->
                    Element.row
                        [ Element.height Element.fill
                        , Element.width Element.fill
                        ]
                        [ viewNav model page, Element.none ]

                Loaded page document ->
                    Element.el
                        [ Element.height Element.fill
                        , Element.width Element.fill
                        , Element.inFront (Element.el [ Element.alignLeft, Element.height Element.fill ] (viewNav model page))
                        ]
                        (viewDocument model page document)

                Error message ->
                    Element.text message
        ]
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
                    handleNewUrl model.rootPath model.pages model.screen url
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
    { title : String
    , readmeUrl : String
    , pages : List { title : String, widgets : List ( String, Widget ) }
    }
    -> Program
program { title, readmeUrl, pages } =
    let
        readmePage =
            Page.readme { url = readmeUrl }

        allPages =
            readmePage :: List.map Page.with pages
    in
    Browser.application
        { init = init title readmePage allPages
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
