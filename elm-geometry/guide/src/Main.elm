module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Events as Events
import Element.Input as Input
import Guide.Document as Document exposing (Document)
import Guide.Widget as Widget exposing (Widget)
import Html exposing (Html)
import Test


type alias Flags =
    ()


type Model
    = Loaded Document
    | Error String


type alias Msg =
    Document


type CounterMsg
    = Increment
    | Decrement


counter : Widget
counter =
    Widget.interactive
        { init = 5
        , view =
            \count ->
                Element.row [ Element.width Element.fill ]
                    [ Element.row [ Element.centerX ]
                        [ Input.button [] { onPress = Just Increment, label = Element.text "+" }
                        , Element.text (String.fromInt count)
                        , Input.button [] { onPress = Just Decrement, label = Element.text "-" }
                        ]
                    ]
        , update =
            \message model ->
                case message of
                    Increment ->
                        model + 1

                    Decrement ->
                        model - 1
        }


init : () -> ( Model, Cmd Msg )
init flags =
    let
        widgets =
            [ ( "counter", counter )
            ]

        model =
            case Document.parse widgets Test.md of
                Ok document ->
                    Loaded document

                Err message ->
                    Error message
    in
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model of
        Loaded document ->
            { title = Document.title document
            , body =
                [ Element.layout [ Element.width Element.fill ]
                    (Document.view [ Element.width (Element.px 640), Element.centerX ] document)
                ]
            }

        Error message ->
            { title = "Error!"
            , body = [ Html.text message ]
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update newDocument model =
    ( Loaded newDocument, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
