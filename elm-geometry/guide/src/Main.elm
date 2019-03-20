module Main exposing (main)

import Element exposing (Element)
import Element.Events as Events
import Element.Input as Input
import Guide
import Guide.Widget as Widget exposing (Widget)


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


main : Guide.Program
main =
    Guide.program
        { title = "elm-geometry"
        , readmeUrl = "https://cdn.jsdelivr.net/gh/ianmackenzie/elm-geometry@coordinate-systems/README.md"
        , pages = [ { title = "Units and coordinate systems", widgets = [] } ]
        }
