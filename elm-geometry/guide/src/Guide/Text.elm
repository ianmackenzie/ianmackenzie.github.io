module Guide.Text exposing (Kind(..), Location(..), codeFontFamily, fontFamily, fontSize, lineSpacing)

import Element
import Element.Font as Font
import Guide.Screen as Screen


type Location
    = Title
    | Section
    | Subsection
    | Body
    | CodeBlock
    | NavTitle
    | NavBody


type Kind
    = Prose
    | Code


lineSpacing : Screen.Class -> Location -> Element.Attribute msg
lineSpacing screenClass location =
    Element.spacing <|
        case screenClass of
            Screen.Large ->
                case location of
                    Title ->
                        4

                    Section ->
                        4

                    Subsection ->
                        4

                    Body ->
                        8

                    NavTitle ->
                        4

                    NavBody ->
                        8

                    CodeBlock ->
                        6

            Screen.Small ->
                case location of
                    Title ->
                        4

                    Section ->
                        4

                    Subsection ->
                        4

                    Body ->
                        8

                    NavTitle ->
                        4

                    NavBody ->
                        8

                    CodeBlock ->
                        6


fontFamily : Location -> Element.Attribute msg
fontFamily location =
    case location of
        Title ->
            headingFontFamily

        Section ->
            headingFontFamily

        Subsection ->
            headingFontFamily

        Body ->
            bodyFontFamily

        CodeBlock ->
            codeFontFamily

        NavTitle ->
            headingFontFamily

        NavBody ->
            bodyFontFamily


fontSize : Screen.Class -> Location -> Kind -> Int
fontSize screenClass location kind =
    case screenClass of
        Screen.Large ->
            case ( location, kind ) of
                ( Title, Prose ) ->
                    32

                ( Title, Code ) ->
                    32

                ( Section, Prose ) ->
                    24

                ( Section, Code ) ->
                    24

                ( Subsection, Prose ) ->
                    19

                ( Subsection, Code ) ->
                    19

                ( Body, Prose ) ->
                    16

                ( Body, Code ) ->
                    14

                ( CodeBlock, _ ) ->
                    14

                ( NavTitle, _ ) ->
                    16

                ( NavBody, _ ) ->
                    16

        Screen.Small ->
            case ( location, kind ) of
                ( Title, Prose ) ->
                    32

                ( Title, Code ) ->
                    32

                ( Section, Prose ) ->
                    24

                ( Section, Code ) ->
                    24

                ( Subsection, Prose ) ->
                    19

                ( Subsection, Code ) ->
                    19

                ( Body, Prose ) ->
                    16

                ( Body, Code ) ->
                    14

                ( CodeBlock, _ ) ->
                    14

                ( NavTitle, _ ) ->
                    24

                ( NavBody, _ ) ->
                    19


codeFontFamily : Element.Attribute msg
codeFontFamily =
    Font.family
        [ Font.typeface "Source Code Pro"
        , Font.monospace
        ]


headingFontFamily : Element.Attribute msg
headingFontFamily =
    Font.family
        [ Font.typeface "Source Sans Pro"
        , Font.sansSerif
        ]


bodyFontFamily : Element.Attribute msg
bodyFontFamily =
    Font.family
        [ Font.typeface "Source Sans Pro"
        , Font.sansSerif
        ]
