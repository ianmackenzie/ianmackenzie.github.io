module Guide exposing (Program, program)

{-|

@docs Config, app

-}

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation as Navigation
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Guide.Color as Color
import Guide.Document as Document exposing (Document)
import Guide.Font as Font
import Guide.Page as Page exposing (Page)
import Guide.Screen as Screen
import Guide.Widget exposing (Widget)
import Html.Attributes
import Http
import Set exposing (Set)
import Task
import Url exposing (Url)
import Url.Builder


type alias Flags =
    { width : Int
    , height : Int
    }


navWidth : Int
navWidth =
    300


handleMarkdown : Screen.Class -> Page -> Maybe String -> String -> Result Http.Error String -> Msg
handleMarkdown screenClass page fragment rootUrl result =
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
                Ok ( document, imagesToLoad ) ->
                    DocumentLoaded
                        { page = page
                        , fragment = fragment
                        , document = document
                        , imagesToLoad = imagesToLoad
                        }

                Err message ->
                    LoadError message

        Err _ ->
            LoadError "Network error"


appendSlash : String -> String
appendSlash url =
    if String.endsWith "/" url then
        url

    else
        url ++ "/"


loadPage : Screen.Class -> List String -> Page -> Maybe String -> Cmd Msg
loadPage screenClass rootPath page fragment =
    let
        rootUrl =
            Url.Builder.absolute rootPath [] |> appendSlash
    in
    Http.get
        { url = Page.sourceUrl rootPath page
        , expect = Http.expectString (handleMarkdown screenClass page fragment rootUrl)
        }


type State
    = Navigating
    | Loading
    | Error String
    | Loaded
        { page : Page
        , fragment : Maybe String
        , document : Document
        , imagesToLoad : Set String
        }


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
    | DocumentLoaded
        { page : Page
        , fragment : Maybe String
        , document : Document
        , imagesToLoad : Set String
        }
    | DocumentMsg Document.Msg
    | NoOp


type alias Program =
    Platform.Program Flags Model Msg


topLevelDocumentId : String
topLevelDocumentId =
    "top-level-document-e41c28257f09"


handleNewUrl : State -> List String -> Page -> List Page -> Screen.Class -> Url -> ( State, Cmd Msg )
handleNewUrl currentState rootPath readmePage allPages screenClass url =
    case Page.matching { url = url, rootPath = rootPath } allPages of
        Ok (Page.Match page) ->
            let
                loadNewPage =
                    loadPage screenClass rootPath page url.fragment

                command =
                    case currentState of
                        Loaded current ->
                            if page == current.page then
                                scrollTo url.fragment

                            else
                                loadNewPage

                        _ ->
                            loadNewPage
            in
            ( currentState, command )

        Ok Page.Unspecified ->
            case screenClass of
                Screen.Large ->
                    let
                        loadReadme =
                            loadPage screenClass rootPath readmePage url.fragment
                    in
                    ( currentState
                    , case currentState of
                        Loaded current ->
                            if Page.isReadme current.page then
                                scrollTo url.fragment

                            else
                                loadReadme

                        _ ->
                            loadReadme
                    )

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


scrollTo : Maybe String -> Cmd Msg
scrollTo fragment =
    let
        scrollTask =
            case fragment of
                Just id ->
                    Browser.Dom.getElement id
                        |> Task.andThen (\{ element } -> Browser.Dom.setViewport 0 element.y)

                Nothing ->
                    Browser.Dom.setViewport 0 0
    in
    scrollTask
        |> Task.attempt
            (\result ->
                case result of
                    Ok () ->
                        NoOp

                    Err (Browser.Dom.NotFound id) ->
                        LoadError ("Could not find element on page with id '" ++ id ++ "'")
            )


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
        , widthFill
        ]
        [ Element.text model.packageName ]


packageDocLink : Model -> Element msg
packageDocLink model =
    Element.row [ Element.spacing 4 ]
        [ navIcon model { src = "images/elm.svg", description = "Elm logo" }
        , Element.link []
            { url =
                Url.Builder.crossOrigin "https://package.elm-lang.org"
                    [ "packages", model.author, model.packageName, "latest" ]
                    []
            , label = Element.text "Package documentation"
            }
        ]


gitHubLink : Model -> Element msg
gitHubLink model =
    Element.row [ Element.spacing 4 ]
        [ navIcon model { src = "images/github.svg", description = "GitHub logo" }
        , Element.link []
            { url =
                Url.Builder.crossOrigin "https://github.com"
                    [ model.author, model.packageName ]
                    []
            , label = Element.text "Source code"
            }
        ]


navIcon : Model -> { src : String, description : String } -> Element msg
navIcon model properties =
    Element.image [ Element.height (Element.px (Font.sizes model.screenClass).navText) ] properties


horizontalDivider : Element msg
horizontalDivider =
    Element.el
        [ widthFill
        , Element.height (Element.px 1)
        , Background.color Color.dividerLine
        ]
        Element.none


viewNav : Model -> Maybe Page -> Element msg
viewNav model currentPage =
    let
        navElement =
            Element.column
                [ heightFill
                , widthFill
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
                    , [ horizontalDivider
                      , packageDocLink model
                      , gitHubLink model
                      ]
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
        [ heightFill
        , Element.width elementWidth
        , Element.clipY
        , Element.scrollbarY
        ]
        navElement


viewDocument : Document -> Element Msg
viewDocument loadedDocument =
    Document.view [ Element.centerX ] loadedDocument
        |> Element.map DocumentMsg


widthFill : Element.Attribute msg
widthFill =
    Element.width Element.fill


heightFill : Element.Attribute msg
heightFill =
    Element.height Element.fill


view : Model -> Browser.Document Msg
view model =
    { title = model.packageName
    , body =
        [ case model.screenClass of
            Screen.Large ->
                let
                    displayedPage =
                        case model.state of
                            Loaded { page } ->
                                Just page

                            Navigating ->
                                Just model.readmePage

                            Loading ->
                                Nothing

                            Error _ ->
                                Nothing

                    navElement =
                        Element.el [ Element.alignLeft, heightFill ]
                            (viewNav model displayedPage)
                in
                Element.layout [ widthFill, heightFill, Element.inFront navElement ] <|
                    case model.state of
                        Navigating ->
                            Element.none

                        Loading ->
                            Element.none

                        Loaded { document } ->
                            viewDocument document

                        Error message ->
                            Element.el [ Element.centerX ] (Element.text message)

            Screen.Small ->
                Element.layout [ widthFill, heightFill ] <|
                    case model.state of
                        Navigating ->
                            viewNav model Nothing

                        Loading ->
                            Element.none

                        Loaded { document } ->
                            viewDocument document

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

        DocumentLoaded loaded ->
            ( { model | state = Loaded loaded }
            , if Set.isEmpty loaded.imagesToLoad then
                -- No images to load, can immediately scroll to given fragment
                scrollTo loaded.fragment

              else
                Cmd.none
            )

        DocumentMsg (Document.Updated newDocument) ->
            case model.state of
                Loaded current ->
                    ( { model | state = Loaded { current | document = newDocument } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DocumentMsg (Document.ImageLoaded url) ->
            case model.state of
                Loaded current ->
                    let
                        updatedImagesToLoad =
                            Set.remove url current.imagesToLoad
                    in
                    ( { model | state = Loaded { current | imagesToLoad = updatedImagesToLoad } }
                    , if Set.isEmpty updatedImagesToLoad then
                        -- All images loaded, scroll to given fragment
                        scrollTo current.fragment

                      else
                        Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
