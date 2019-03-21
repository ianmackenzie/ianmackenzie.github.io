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
import Guide.Screen as Screen
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


navWidth : Int
navWidth =
    300


handleMarkdown : Screen.Class -> Page -> String -> Result Http.Error String -> Msg
handleMarkdown screenClass page rootUrl result =
    case result of
        Ok markdown ->
            let
                documentConfig =
                    { screenClass = screenClass
                    , widgets = []
                    , rootUrl = rootUrl
                    }
            in
            case Document.parse documentConfig markdown of
                Ok document ->
                    DocumentLoaded page document

                Err message ->
                    LoadError message

        Err error ->
            LoadError "Network error"


loadPage : Screen.Class -> List String -> Page -> Cmd Msg
loadPage screenClass rootPath page =
    let
        rootUrl =
            Url.Builder.absolute rootPath []
    in
    Http.get
        { url = Page.sourceUrl rootPath page
        , expect = Http.expectString (handleMarkdown screenClass page rootUrl)
        }


type State
    = Navigating
    | Loading
    | Error String
    | Loaded Page Document


type alias Model =
    { screenClass : Screen.Class
    , navigationKey : Navigation.Key
    , rootPath : List String
    , author : String
    , packageName : String
    , readmePage : Page
    , allPages : List Page
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


handleNewUrl : State -> List String -> Page -> List Page -> Screen.Class -> Url -> ( State, Cmd Msg )
handleNewUrl currentState rootPath readmePage allPages screenClass url =
    case Debug.log "match" (Page.matching { url = url, rootPath = rootPath } allPages) of
        Ok (Page.Match { page, fragment }) ->
            ( currentState, loadPage screenClass rootPath page )

        Ok Page.Unspecified ->
            case screenClass of
                Screen.Large ->
                    ( currentState, loadPage screenClass rootPath readmePage )

                Screen.Small ->
                    ( Navigating, Cmd.none )

        Err (Page.NotFound title) ->
            let
                errorMessage =
                    "Page \"" ++ title ++ "\" not found"
            in
            ( Error errorMessage, Cmd.none )

        Err Page.BadUrl ->
            let
                errorMessage =
                    "No page matching " ++ Url.toString url
            in
            ( Error errorMessage, Cmd.none )


init : String -> String -> Page -> List Page -> Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init author packageName readmePage allPages flags url navigationKey =
    let
        screenClass =
            Screen.classify flags.width

        rootPath =
            url.path |> String.split "/" |> List.filter (not << String.isEmpty)

        ( state, command ) =
            handleNewUrl Loading rootPath readmePage allPages screenClass url
    in
    ( { author = author
      , packageName = packageName
      , readmePage = readmePage
      , allPages = allPages
      , state = state
      , screenClass = screenClass
      , navigationKey = navigationKey
      , rootPath = rootPath
      }
    , command
    )


toPageLink : List String -> Maybe Page -> Page -> Element msg
toPageLink rootPath currentPage page =
    let
        attributes =
            if currentPage == Just page then
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
    Element.paragraph
        [ Font.color Color.black
        , Font.bold
        , Font.size (Font.sizes model.screenClass).navTitle
        , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
        , Element.paddingEach { top = 0, bottom = 8, left = 0, right = 0 }
        , Border.color Color.dividerLine
        , Element.width Element.fill
        ]
        [ Element.text model.packageName ]


packageDocLink : Model -> Element msg
packageDocLink model =
    Element.paragraph
        [ Font.alegreyaSans
        , Font.color Color.linkText
        , Font.size (Font.sizes model.screenClass).navText
        , Font.regular
        , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        , Element.paddingEach { top = 8, bottom = 0, left = 0, right = 0 }
        , Border.color Color.dividerLine
        , Element.width Element.fill
        ]
        [ Element.link []
            { url =
                Url.Builder.crossOrigin "https://package.elm-lang.org"
                    [ "packages"
                    , model.author
                    , model.packageName
                    , "latest"
                    ]
                    []
            , label = Element.text "Package documentation"
            }
        ]


viewNav : Model -> Maybe Page -> Element msg
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
                , Font.size (Font.sizes model.screenClass).navText
                , Font.regular
                , Element.spacing 12
                , Element.padding 12
                ]
                (List.concat
                    [ [ navTitle model ]
                    , List.map (toPageLink model.rootPath currentPage) model.allPages
                    , [ packageDocLink model ]
                    ]
                )

        elementWidth =
            case model.screenClass of
                Screen.Large ->
                    Element.px navWidth

                Screen.Small ->
                    Element.fill
    in
    Element.el
        [ Element.height Element.fill
        , Element.width elementWidth
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
    { title = model.packageName
    , body =
        [ Element.layout [ Element.width Element.fill, Element.height Element.fill ] <|
            case model.screenClass of
                Screen.Large ->
                    case model.state of
                        Navigating ->
                            Element.row
                                [ Element.height Element.fill
                                , Element.width Element.fill
                                ]
                                [ viewNav model (Just model.readmePage), Element.none ]

                        Loading ->
                            Element.row
                                [ Element.height Element.fill
                                , Element.width Element.fill
                                ]
                                [ viewNav model Nothing, Element.none ]

                        Loaded page document ->
                            Element.el
                                [ Element.height Element.fill
                                , Element.width Element.fill
                                , Element.inFront (Element.el [ Element.alignLeft, Element.height Element.fill ] (viewNav model (Just page)))
                                ]
                                (viewDocument model page document)

                        Error message ->
                            Element.text message

                Screen.Small ->
                    case model.state of
                        Navigating ->
                            viewNav model Nothing

                        Loading ->
                            Element.none

                        Loaded page document ->
                            viewDocument model page document

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
                ( newState, command ) =
                    handleNewUrl
                        model.state
                        model.rootPath
                        model.readmePage
                        model.allPages
                        model.screenClass
                        url
            in
            ( { model | state = newState }, command )

        LoadError string ->
            ( { model | state = Error string }, Cmd.none )

        DocumentLoaded documentPage document ->
            ( { model | state = Loaded documentPage document }, Cmd.none )

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
    { author : String
    , packageName : String
    , readmeUrl : String
    , pages : List { title : String, widgets : List ( String, Widget ) }
    }
    -> Program
program { author, packageName, readmeUrl, pages } =
    let
        readmePage =
            Page.readme { url = readmeUrl }

        allPages =
            readmePage :: List.map Page.with pages
    in
    Browser.application
        { init = init author packageName readmePage allPages
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
