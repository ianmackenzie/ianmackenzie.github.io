module Guide exposing (Program, program)

{-|

@docs Config, app

-}

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Elm.Docs
import Guide.Color as Color
import Guide.Document as Document exposing (Document)
import Guide.Page as Page exposing (Page)
import Guide.Screen as Screen
import Guide.Text as Text
import Guide.Widget exposing (Widget)
import Html.Attributes
import Http
import Json.Decode as Decode
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Task
import Time
import Url exposing (Url)
import Url.Builder


type alias Flags =
    { width : Int
    , height : Int
    }


navWidth : Int
navWidth =
    300


monthName : Time.Month -> String
monthName month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


toDateString : Int -> Time.Month -> Int -> Int -> Int -> Int -> String
toDateString day month year _ _ _ =
    monthName month ++ " " ++ String.fromInt day ++ ", " ++ String.fromInt year


parseTwoDigitNumber : Parser Int
parseTwoDigitNumber =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.token "0"
            |= Parser.int
        , Parser.int
        ]


dateParser : Parser String
dateParser =
    -- From MDN: "Last-Modified: <day-name>, <day> <month> <year> <hour>:<minute>:<second> GMT"
    -- https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Last-Modified
    Parser.succeed toDateString
        |. Parser.oneOf (List.map Parser.token [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ])
        |. Parser.token ", "
        |= parseTwoDigitNumber
        |. Parser.token " "
        |= Parser.oneOf
            [ Parser.map (always Time.Jan) (Parser.token "Jan")
            , Parser.map (always Time.Feb) (Parser.token "Feb")
            , Parser.map (always Time.Mar) (Parser.token "Mar")
            , Parser.map (always Time.Apr) (Parser.token "Apr")
            , Parser.map (always Time.May) (Parser.token "May")
            , Parser.map (always Time.Jun) (Parser.token "Jun")
            , Parser.map (always Time.Jul) (Parser.token "Jul")
            , Parser.map (always Time.Aug) (Parser.token "Aug")
            , Parser.map (always Time.Sep) (Parser.token "Sep")
            , Parser.map (always Time.Oct) (Parser.token "Oct")
            , Parser.map (always Time.Nov) (Parser.token "Nov")
            , Parser.map (always Time.Dec) (Parser.token "Dec")
            ]
        |. Parser.token " "
        |= Parser.int
        |. Parser.token " "
        |= parseTwoDigitNumber
        |. Parser.token ":"
        |= parseTwoDigitNumber
        |. Parser.token ":"
        |= parseTwoDigitNumber
        |. Parser.token " GMT"


handleResponse : Http.Response String -> Result String { markdown : String, lastModified : Maybe String }
handleResponse response =
    case response of
        Http.BadUrl_ url ->
            Err ("Could not load page from " ++ url)

        Http.Timeout_ ->
            Err "HTTP request timed out when loading page"

        Http.NetworkError_ ->
            Err "HTTP network error when loading page"

        Http.BadStatus_ metadata _ ->
            Err ("Got an HTTP " ++ String.fromInt metadata.statusCode ++ " error code when loading page")

        Http.GoodStatus_ metadata content ->
            let
                parsedDate =
                    Dict.get "last-modified" metadata.headers
                        |> Maybe.map
                            (\lastModified ->
                                case Parser.run dateParser lastModified of
                                    Ok parsed ->
                                        parsed

                                    Err _ ->
                                        lastModified
                            )
            in
            Ok { markdown = content, lastModified = parsedDate }


handleMarkdown : Page -> Maybe String -> Dict String Widget -> Result String { markdown : String, lastModified : Maybe String } -> Msg
handleMarkdown page fragment widgets result =
    case result of
        Ok { markdown, lastModified } ->
            case Document.parse widgets markdown of
                Ok ( document, imagesToLoad ) ->
                    DocumentLoaded
                        { page = page
                        , fragment = fragment
                        , document = document
                        , lastModified = lastModified
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


loadPage : List String -> Page -> Maybe String -> Dict String Widget -> Cmd Msg
loadPage rootPath page fragment widgets =
    Http.get
        { url = Page.sourceUrl rootPath page
        , expect = Http.expectStringResponse (handleMarkdown page fragment widgets) handleResponse
        }


type State
    = Navigating
    | Loading
    | Error String
    | Loaded
        { page : Page
        , fragment : Maybe String
        , document : Document
        , lastModified : Maybe String
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
    , moduleNames : Set String
    , widgets : Dict String Widget
    }


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | LoadError String
    | DocumentLoaded
        { page : Page
        , fragment : Maybe String
        , document : Document
        , lastModified : Maybe String
        , imagesToLoad : Set String
        }
    | DocumentMsg Document.Msg
    | DocsJsonResponse (Result Http.Error (List String))
    | NoOp


type alias Program =
    Platform.Program Flags Model Msg


handleNewUrl : State -> List String -> Page -> List Page -> Screen.Class -> Url -> Dict String Widget -> ( State, Cmd Msg )
handleNewUrl currentState rootPath readmePage allPages screenClass url widgets =
    case Page.matching { url = url, rootPath = rootPath } allPages of
        Ok (Page.Match page) ->
            let
                loadNewPage =
                    loadPage rootPath page url.fragment widgets

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
                            loadPage rootPath readmePage url.fragment widgets
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
    case fragment of
        Just id ->
            Browser.Dom.getElement id
                |> Task.andThen (\{ element } -> Browser.Dom.setViewport 0 element.y)
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok () ->
                                NoOp

                            Err (Browser.Dom.NotFound _) ->
                                LoadError ("Could not find element on page with id '" ++ id ++ "'")
                    )

        Nothing ->
            Cmd.none


init : String -> String -> List ( String, Widget ) -> Page -> List Page -> Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init author packageName givenWidgets readmePage allPages flags url navigationKey =
    let
        screenClass =
            Screen.classify flags.width

        rootPath =
            url.path |> String.split "/" |> List.filter (not << String.isEmpty)

        widgets =
            Dict.fromList givenWidgets

        ( state, pageCommand ) =
            handleNewUrl Loading rootPath readmePage allPages screenClass url widgets

        moduleNamesDecoder =
            Decode.list (Elm.Docs.decoder |> Decode.map .name)

        loadDocs =
            Http.get
                { url = "docs.json"
                , expect = Http.expectJson DocsJsonResponse moduleNamesDecoder
                }
    in
    ( { author = author
      , packageName = packageName
      , readmePage = readmePage
      , allPages = allPages
      , state = state
      , screenClass = screenClass
      , navigationKey = navigationKey
      , rootPath = rootPath
      , moduleNames = Set.empty
      , widgets = widgets
      }
    , Cmd.batch [ pageCommand, loadDocs ]
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


packageDocLink : Model -> Element msg
packageDocLink model =
    Element.row [ Element.spacing 4 ]
        [ navIcon model { src = "images/elm.svg", description = "Elm logo" }
        , Element.link []
            { url =
                Url.Builder.crossOrigin "https://package.elm-lang.org"
                    [ "packages", model.author, model.packageName, "latest" ]
                    []
            , label = Element.text "API documentation"
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
    Element.image [ Element.height (Element.px (Text.fontSize model.screenClass Text.NavBody Text.Prose)) ] properties


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
        titleElement =
            Element.paragraph
                [ Font.color Color.navTitle
                , Font.size (Text.fontSize model.screenClass Text.NavTitle Text.Prose)
                ]
                [ Element.text "The "
                , Element.el [ Text.codeFontFamily ] (Element.text model.packageName)
                , Element.text " guide"
                ]

        navPadding =
            case model.screenClass of
                Screen.Large ->
                    24

                Screen.Small ->
                    12

        navElement =
            Element.column
                [ heightFill
                , widthFill
                , Background.color Color.white
                , Text.fontFamily Text.Body
                , Font.color Color.linkText
                , Font.size (Text.fontSize model.screenClass Text.NavBody Text.Prose)
                , Font.regular
                , Element.spacing 12
                , Element.padding navPadding
                ]
                (List.concat
                    [ [ titleElement ]
                    , [ horizontalDivider ]
                    , List.map (toPageLink model.rootPath currentPage) model.allPages
                    , [ horizontalDivider ]
                    , [ packageDocLink model ]
                    , [ gitHubLink model ]
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


maxDocumentWidth : Int
maxDocumentWidth =
    640


viewDocument : Model -> List (Element.Attribute Never) -> Document -> Maybe String -> Element Msg
viewDocument model attributes loadedDocument lastModified =
    let
        documentConfig =
            { screenClass = model.screenClass
            , lastModified = lastModified
            , rootUrl = Url.Builder.absolute model.rootPath [] |> appendSlash
            , moduleNames = model.moduleNames
            , author = model.author
            , packageName = model.packageName
            }
    in
    Document.view documentConfig (List.map (Element.mapAttribute never) attributes) loadedDocument
        |> Element.map DocumentMsg


widthFill : Element.Attribute msg
widthFill =
    Element.width Element.fill


heightFill : Element.Attribute msg
heightFill =
    Element.height Element.fill


navDivider : List (Element.Attribute msg) -> Element msg
navDivider givenAttributes =
    Element.el [ Element.paddingXY 0 16, heightFill ] <|
        Element.el (heightFill :: Element.width (Element.px 1) :: givenAttributes) Element.none


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

                    navLayer =
                        Element.row
                            [ widthFill
                            , heightFill
                            , Element.htmlAttribute (Html.Attributes.style "pointer-events" "none")
                            ]
                            [ Element.el
                                [ widthFill ]
                                Element.none
                            , Element.el
                                [ Element.width (Element.px maxDocumentWidth)
                                , heightFill
                                ]
                                Element.none
                            , navDivider [ Background.color Color.dividerLine ]
                            , Element.el
                                [ Element.width (Element.fill |> Element.minimum navWidth)
                                , heightFill
                                , Element.htmlAttribute (Html.Attributes.style "pointer-events" "auto")
                                ]
                                (Element.el [ Element.alignLeft, heightFill ]
                                    (viewNav model displayedPage)
                                )
                            ]

                    documentElement =
                        case model.state of
                            Navigating ->
                                Element.none

                            Loading ->
                                Element.none

                            Loaded { document, lastModified } ->
                                viewDocument model [ widthFill, heightFill ] document lastModified

                            Error message ->
                                Element.el [ Element.centerX ] (Element.text message)

                    mainLayer =
                        Element.row [ widthFill, heightFill ]
                            [ Element.el
                                [ widthFill ]
                                Element.none
                            , Element.el
                                [ Element.width (Element.px maxDocumentWidth)
                                , heightFill
                                , Element.paddingXY 24 12
                                ]
                                documentElement
                            , navDivider []
                            , Element.el
                                [ Element.width (Element.fill |> Element.minimum navWidth)
                                , heightFill
                                ]
                                Element.none
                            ]
                in
                Element.layout [ widthFill, heightFill, Element.inFront navLayer ] mainLayer

            Screen.Small ->
                Element.layout [ widthFill, heightFill ] <|
                    case model.state of
                        Navigating ->
                            viewNav model Nothing

                        Loading ->
                            Element.none

                        Loaded { document, lastModified } ->
                            viewDocument
                                model
                                [ Element.padding 12
                                , Element.width (Element.fill |> Element.maximum 10000)
                                ]
                                document
                                lastModified

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
                    ( model, Navigation.load (Url.toString url) )

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
                        model.widgets
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
                -- Wait for images to load
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

        DocsJsonResponse (Ok moduleNames) ->
            ( { model | moduleNames = Set.fromList moduleNames }, Cmd.none )

        DocsJsonResponse (Err _) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


program :
    { author : String
    , packageName : String
    , branch : String
    , pages : List String
    , widgets : List ( String, Widget )
    }
    -> Program
program { author, packageName, branch, pages, widgets } =
    let
        readmeUrl =
            Url.Builder.crossOrigin "https://cdn.jsdelivr.net"
                [ "gh"
                , author
                , packageName ++ "@" ++ branch
                , "README.md"
                ]
                []

        readmePage =
            Page.readme { url = readmeUrl }

        toPage title =
            Page.with { title = title, widgets = widgets }

        allPages =
            readmePage :: List.map toPage pages
    in
    Browser.application
        { init = init author packageName widgets readmePage allPages
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
