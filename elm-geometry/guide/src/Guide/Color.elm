module Guide.Color exposing
    ( black
    , codeBlockBackground
    , dividerLine
    , inlineCodeBackground
    , lastModified
    , linkText
    , navBackground
    , navBorder
    , navDividerLine
    , navTitle
    , white
    )

import Element


codeBlockBackground : Element.Color
codeBlockBackground =
    Element.rgb255 245 245 245


lightGrey : Element.Color
lightGrey =
    Element.rgb255 238 238 238


inlineCodeBackground : Element.Color
inlineCodeBackground =
    lightGrey


dividerLine : Element.Color
dividerLine =
    lightGrey


linkText : Element.Color
linkText =
    -- originally 17 131 204
    Element.rgb255 14 105 163


navBackground : Element.Color
navBackground =
    lightGrey


elmLightBlue : Element.Color
elmLightBlue =
    Element.rgb255 96 181 204


navBorder : Element.Color
navBorder =
    lightGrey


white : Element.Color
white =
    Element.rgb255 255 255 255


black : Element.Color
black =
    Element.rgb255 0 0 0


navDividerLine : Element.Color
navDividerLine =
    Element.rgb255 200 200 200


grey : Element.Color
grey =
    Element.rgb 0.5 0.5 0.5


lastModified : Element.Color
lastModified =
    grey


navTitle : Element.Color
navTitle =
    grey
