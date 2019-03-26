module Main exposing (main)

import Guide


main : Guide.Program
main =
    Guide.program
        { author = "ianmackenzie"
        , packageName = "elm-geometry"
        , readmeUrl = "https://cdn.jsdelivr.net/gh/ianmackenzie/elm-geometry@coordinate-systems/README.md"
        , pages = [ { title = "Units and coordinate systems", widgets = [] } ]
        }
