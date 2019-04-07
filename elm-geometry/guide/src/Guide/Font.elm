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
        [ Element.Font.typeface "Source Sans Pro"
        , Element.Font.sansSerif
        ]


body : Element.Attribute msg
body =
    Element.Font.family
        [ Element.Font.typeface "Source Sans Pro"
        , Element.Font.sansSerif
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
    , lastModified : Int
    }


largeScreenSizes : Sizes
largeScreenSizes =
    { title = 32
    , titleLineSpacing = 4
    , section = 24
    , sectionLineSpacing = 4
    , subsection = 19
    , subsectionLineSpacing = 4
    , body = 16
    , bodyLineSpacing = 8
    , titleCode = 32
    , sectionCode = 24
    , subsectionCode = 19
    , bodyCode = 14
    , codeBlockCode = 14
    , codeBlockLineSpacing = 4
    , navText = 16
    , navTitle = 16
    , lastModified = 16
    }


smallScreenSizes : Sizes
smallScreenSizes =
    { title = 32
    , titleLineSpacing = 4
    , section = 22
    , sectionLineSpacing = 4
    , subsection = 19
    , subsectionLineSpacing = 4
    , body = 16
    , bodyLineSpacing = 6
    , titleCode = 32
    , sectionCode = 22
    , subsectionCode = 19
    , bodyCode = 14
    , codeBlockCode = 14
    , codeBlockLineSpacing = 4
    , navText = 19
    , navTitle = 19
    , lastModified = 16
    }


sizes : Screen.Class -> Sizes
sizes screenClass =
    case screenClass of
        Screen.Large ->
            largeScreenSizes

        Screen.Small ->
            smallScreenSizes
