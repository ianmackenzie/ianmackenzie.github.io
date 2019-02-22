module Guide.Screen exposing (Class(..), classify)


type Class
    = Small
    | Large


classify : { width : Int } -> Class
classify { width } =
    if width > 600 then
        Large

    else
        Small
