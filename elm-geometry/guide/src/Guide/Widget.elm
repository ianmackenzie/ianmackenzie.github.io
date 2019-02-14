module Guide.Widget exposing (Widget, interactive, static, view)

import Element exposing (Element)


type Widget
    = Widget (Element Widget)


static : Element Never -> Widget
static element =
    Widget (Element.map never element)


interactive : { init : model, view : model -> Element msg, update : msg -> model -> model } -> Widget
interactive config =
    Widget
        (config.view config.init
            |> Element.map
                (\message ->
                    interactive
                        { init = config.update message config.init
                        , view = config.view
                        , update = config.update
                        }
                )
        )


view : (Widget -> msg) -> Widget -> Element msg
view toMessage (Widget element) =
    Element.map toMessage element
