module Guide exposing (Config, Widget, app, parse, widget)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Element exposing (Element)
import Html exposing (Html)
import Markdown.Block as Block exposing (Block)
import Markdown.Config
import Markdown.Inline as Inline exposing (Inline)
import Parser
import Parser.Advanced
import Url exposing (Url)


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url


type alias Program =
    Platform.Program () Model Msg


type alias Renderer =
    List ( String, Maybe String ) -> Result String Widget


type alias Plugin =
    { tag : String
    , render : List ( String, Maybe String ) -> Result String Widget
    }


type alias Config =
    { title : String
    , blocks : List Plugin
    }


init : Config -> Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init config flags url navigationKey =
    ( (), Cmd.none )


view : Config -> Model -> Browser.Document Msg
view config model =
    { title = config.title
    , body =
        [ Html.text "Hello" ]
    }


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update config message model =
    ( model, Cmd.none )


subscriptions : Config -> Model -> Sub Msg
subscriptions config model =
    Sub.none


app : Config -> Program
app config =
    Browser.application
        { init = init config
        , view = view config
        , update = update config
        , subscriptions = subscriptions config
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type Widget
    = Widget (Element Widget)


widget : { init : a, view : a -> Element a } -> Widget
widget config =
    Widget
        (config.view config.init
            |> Element.map (\value -> widget { init = value, view = config.view })
        )


type Text
    = Plain String
    | Italic String
    | Bold String
    | InlineCode String


type Chunk
    = Section (List Text)
    | Subsection (List Text)
    | Paragraph (List Text)
    | CustomBlock Widget
    | CodeBlock String


parseText : List (Inline Never) -> List Text -> Result String (List Text)
parseText inlines accumulated =
    case inlines of
        first :: rest ->
            let
                prepend text =
                    parseText rest (text :: accumulated)
            in
            case first of
                Inline.Text string ->
                    prepend (Plain string)

                Inline.HardLineBreak ->
                    Err "Hard line breaks not yet supported"

                Inline.CodeInline string ->
                    prepend (InlineCode string)

                Inline.Link _ _ _ ->
                    Err "Links not yet supported"

                Inline.Image _ _ _ ->
                    Err "Inline images not yet supported"

                Inline.HtmlInline _ _ _ ->
                    Err "Inline custom elements not yet supported"

                Inline.Emphasis 1 [ Inline.Text string ] ->
                    prepend (Italic string)

                Inline.Emphasis 2 [ Inline.Text string ] ->
                    prepend (Bold string)

                Inline.Emphasis _ _ ->
                    Err "Only _italic plain text_ and **bold plain text** are currently supported"

                Inline.Custom _ _ ->
                    Err "Internal error - should not be possible to have a Custom inline"

        [] ->
            Ok (List.reverse accumulated)


parseChunks : Dict String Renderer -> List (Block Never Never) -> List Chunk -> Result String (List Chunk)
parseChunks plugins blocks accumulated =
    case blocks of
        first :: rest ->
            let
                prepend chunk =
                    parseChunks plugins rest (chunk :: accumulated)
            in
            case first of
                Block.BlankLine _ ->
                    parseChunks plugins rest accumulated

                Block.ThematicBreak ->
                    Err "ThematicBreak not yet supported"

                Block.Heading _ level inlines ->
                    case level of
                        1 ->
                            Result.map Section (parseText inlines []) |> Result.andThen prepend

                        2 ->
                            Result.map Subsection (parseText inlines []) |> Result.andThen prepend

                        _ ->
                            Err ("Heading level " ++ String.fromInt level ++ " not yet supported")

                Block.CodeBlock Block.Indented code ->
                    prepend (CodeBlock code)

                Block.CodeBlock (Block.Fenced _ _) code ->
                    -- TODO support different language types
                    prepend (CodeBlock code)

                Block.Paragraph _ inlines ->
                    Result.andThen (Paragraph >> prepend) (parseText inlines [])

                Block.BlockQuote _ ->
                    Err "Block quotes not yet supported"

                Block.List _ _ ->
                    Err "Lists not yet supported"

                Block.PlainInlines inlines ->
                    case inlines of
                        [ inline ] ->
                            case inline of
                                Inline.Image _ _ _ ->
                                    Err "Images not yet supported"

                                Inline.HtmlInline tag attributes [] ->
                                    case Dict.get tag plugins of
                                        Just renderer ->
                                            Result.map CustomBlock (renderer attributes)
                                                |> Result.andThen prepend

                                        Nothing ->
                                            Err ("No plugin found for tag " ++ tag)

                                Inline.HtmlInline tag attributes children ->
                                    Err "Custom elements with children not yet supported"

                                _ ->
                                    Err "Only images and custom elements supported as standalone elements right now"

                        _ ->
                            Err "Only single plain inlines supported"

                Block.Custom _ _ ->
                    Err "Internal error - should not be possible to have a Custom block"

        [] ->
            Ok (List.reverse accumulated)


parse : Config -> String -> Result String (List Chunk)
parse config markdown =
    let
        options =
            { softAsHardLineBreak = False
            , rawHtml = Markdown.Config.ParseUnsafe
            }

        blocks =
            Block.parse (Just options) markdown

        plugins =
            config.blocks
                |> List.map (\{ tag, render } -> ( tag, render ))
                |> Dict.fromList
    in
    parseChunks plugins blocks []
