module Guide exposing (Config, Widget, app, parse, widget)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Element exposing (Element)
import Html exposing (Html)
import Mark
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


type alias Config =
    { title : String
    , blocks : List (Mark.Block Widget)
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
    = Text Mark.Text
    | InlineCode String


stripStyles : Mark.Text -> String
stripStyles (Mark.Text _ string) =
    string


toCode : List Mark.Text -> String
toCode textFragments =
    String.concat (List.map stripStyles textFragments)


paragraph : Mark.Block Chunk
paragraph =
    Mark.text
        { view = Text
        , inlines =
            [ Mark.inline "Elm" (\textFragments -> InlineCode (toCode textFragments))
                |> Mark.inlineText
            ]
        , replacements = []
        }
        |> Mark.map Paragraph


type Chunk
    = Section String
    | Subsection String
    | Paragraph (List Text)
    | CustomBlock Widget
    | CodeBlock String


section : Mark.Block Chunk
section =
    Mark.block "Section" Section Mark.string


subsection : Mark.Block Chunk
subsection =
    Mark.block "Subsection" Subsection Mark.string


codeBlock : Mark.Block Chunk
codeBlock =
    Mark.block "Elm" CodeBlock Mark.multiline


customBlock : Config -> Mark.Block Chunk
customBlock config =
    Mark.map CustomBlock (Mark.oneOf config.blocks)


chunks : Config -> Mark.Block (List Chunk)
chunks config =
    Mark.manyOf
        [ section
        , subsection
        , paragraph
        , customBlock config
        , codeBlock
        ]


document : Config -> Mark.Document (List Chunk)
document config =
    Mark.document identity (chunks config)


parse : Config -> String -> Result (List (Parser.Advanced.DeadEnd Mark.Context Mark.Problem)) (List Chunk)
parse config contents =
    Mark.parse (document config) contents
