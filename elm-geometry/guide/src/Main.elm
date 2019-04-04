module Main exposing (main)

import Guide


main : Guide.Program
main =
    Guide.program
        { author = "ianmackenzie"
        , packageName = "elm-geometry"
        , branch = "coordinate-systems"
        , pages = [ { title = "Units and coordinate systems", widgets = [] } ]
        }
