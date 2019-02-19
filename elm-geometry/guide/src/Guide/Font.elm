module Guide.Font exposing (alegreyaSans, merriweather, sizes, sourceCodePro)

import Element
import Element.Font
import Guide.Screen as Screen


sourceCodePro : Element.Attribute msg
sourceCodePro =
    Element.Font.family
        [ Element.Font.typeface "Source Code Pro"
        , Element.Font.monospace
        ]


alegreyaSans : Element.Attribute msg
alegreyaSans =
    Element.Font.family
        [ Element.Font.typeface "Alegreya Sans"
        , Element.Font.sansSerif
        ]


merriweather : Element.Attribute msg
merriweather =
    Element.Font.family
        [ Element.Font.typeface "Merriweather"
        , Element.Font.serif
        ]


type alias Sizes =
    { title : Int
    , section : Int
    , subsection : Int
    , body : Int
    , titleCode : Int
    , sectionCode : Int
    , subsectionCode : Int
    , bodyCode : Int
    , codeBlockCode : Int
    , navText : Int
    , navTitle : Int
    }


largeScreenSizes : Sizes
largeScreenSizes =
    { title = 48
    , section = 32
    , subsection = 22
    , body = 14
    , titleCode = 44
    , sectionCode = 30
    , subsectionCode = 22
    , bodyCode = 14
    , codeBlockCode = 14
    , navText = 16
    , navTitle = 24
    }


smallScreenSizes : Sizes
smallScreenSizes =
    { title = 32
    , section = 22
    , subsection = 18
    , body = 14
    , titleCode = 28
    , sectionCode = 22
    , subsectionCode = 18
    , bodyCode = 14
    , codeBlockCode = 12
    , navText = 14
    , navTitle = 22
    }


sizes : Screen.Class -> Sizes
sizes screenClass =
    case screenClass of
        Screen.Large ->
            largeScreenSizes

        Screen.Small ->
            smallScreenSizes
