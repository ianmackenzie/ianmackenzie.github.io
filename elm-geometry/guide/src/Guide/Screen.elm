module Guide.Screen exposing (Class(..), Screen, init)


type alias Screen =
    { width : Int
    , class : Class
    }


type Class
    = Small
    | Large


init : { width : Int } -> Screen
init { width } =
    { width = width
    , class =
        if width > 600 then
            Large

        else
            Small
    }
