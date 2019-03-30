module Guide.Font exposing (body, code, heading, sizes)

import Element
import Element.Font
import Guide.Screen as Screen


code : Element.Attribute msg
code =
    Element.Font.family
        [ Element.Font.typeface "Source Code Pro"
        , Element.Font.monospace
        ]


heading : Element.Attribute msg
heading =
    Element.Font.family
        [ Element.Font.typeface "Alegreya Sans"
        , Element.Font.sansSerif
        ]


body : Element.Attribute msg
body =
    Element.Font.family
        [ Element.Font.typeface "Merriweather"
        , Element.Font.serif
        ]


type alias Sizes =
    { title : Int
    , titleLineSpacing : Int
    , section : Int
    , sectionLineSpacing : Int
    , subsection : Int
    , subsectionLineSpacing : Int
    , body : Int
    , bodyLineSpacing : Int
    , titleCode : Int
    , sectionCode : Int
    , subsectionCode : Int
    , bodyCode : Int
    , codeBlockCode : Int
    , codeBlockLineSpacing : Int
    , navText : Int
    , navTitle : Int
    }


largeScreenSizes : Sizes
largeScreenSizes =
    { title = 48
    , titleLineSpacing = 0
    , section = 32
    , sectionLineSpacing = 0
    , subsection = 22
    , subsectionLineSpacing = 0
    , body = 14
    , bodyLineSpacing = 12
    , titleCode = 44
    , sectionCode = 30
    , subsectionCode = 22
    , bodyCode = 14
    , codeBlockCode = 14
    , codeBlockLineSpacing = 4
    , navText = 16
    , navTitle = 24
    }


smallScreenSizes : Sizes
smallScreenSizes =
    { title = 32
    , titleLineSpacing = 0
    , section = 22
    , sectionLineSpacing = 0
    , subsection = 18
    , subsectionLineSpacing = 0
    , body = 14
    , bodyLineSpacing = 10
    , titleCode = 28
    , sectionCode = 22
    , subsectionCode = 18
    , bodyCode = 14
    , codeBlockCode = 12
    , codeBlockLineSpacing = 4
    , navText = 18
    , navTitle = 32
    }


sizes : Screen.Class -> Sizes
sizes screenClass =
    case screenClass of
        Screen.Large ->
            largeScreenSizes

        Screen.Small ->
            smallScreenSizes
