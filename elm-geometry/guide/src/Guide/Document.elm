module Guide.Document exposing (Document, parse, title, view)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Guide.Widget as Widget exposing (Widget)
import Markdown.Block as Block exposing (Block)
import Markdown.Config
import Markdown.Inline as Inline exposing (Inline)
import Regex exposing (Regex)
import Result.Extra as Result
import Svg
import Svg.Attributes


type Document
    = Document
        { title : String
        , chunks : List CompiledChunk
        }


type alias Msg =
    ( Int, Widget )


type CompiledChunk
    = Static TextContext (Element Never)
    | Interactive Int Widget
    | CompiledBullets (List (List CompiledChunk))


type Chunk
    = Title (List Text)
    | Section (List Text)
    | Subsection (List Text)
    | Paragraph (List Text)
    | CustomBlock Widget
    | CodeBlock String
    | Bullets (List (List Chunk))


type InlineCodeChunk
    = Spaces String
    | Characters String


type Text
    = Plain String
    | Italic String
    | Bold String
    | InlineCode (List InlineCodeChunk)


type TextContext
    = TitleContext
    | SectionContext
    | SubsectionContext
    | ParagraphContext
    | CodeBlockContext


widthFill : Element.Attribute msg
widthFill =
    Element.width Element.fill


sourceCodePro : Element.Attribute msg
sourceCodePro =
    Font.family
        [ Font.typeface "Source Code Pro"
        , Font.monospace
        ]


alegreyaSans : Element.Attribute msg
alegreyaSans =
    Font.family
        [ Font.typeface "Alegreya Sans"
        , Font.sansSerif
        ]


merriweather : Element.Attribute msg
merriweather =
    Font.family
        [ Font.typeface "Merriweather"
        , Font.serif
        ]


lightGrey : Element.Color
lightGrey =
    Element.rgb255 238 238 238


fontSizes : { title : Int, section : Int, subsection : Int, body : Int, titleCode : Int, sectionCode : Int, subsectionCode : Int, bodyCode : Int }
fontSizes =
    { title = 48
    , section = 32
    , subsection = 22
    , body = 16
    , titleCode = 44
    , sectionCode = 30
    , subsectionCode = 22
    , bodyCode = 16
    }


topLevelSpacing : Int
topLevelSpacing =
    12


bulletSpacing : Int
bulletSpacing =
    8


title : Document -> String
title (Document document) =
    document.title


view : List (Element.Attribute Document) -> Document -> Element Document
view attributes (Document document) =
    Element.el (merriweather :: Font.size fontSizes.body :: attributes)
        (viewChunks { spacing = topLevelSpacing } document.chunks
            |> Element.map
                (\( id, widget ) ->
                    Document
                        { title = document.title
                        , chunks = updateWidget id widget document.chunks
                        }
                )
        )


viewChunk : CompiledChunk -> Element Msg
viewChunk chunk =
    case chunk of
        Static textContext element ->
            Element.map never element

        Interactive id widget ->
            Widget.view (\newWidget -> ( id, newWidget )) widget

        CompiledBullets bullets ->
            viewBullets bullets


viewChunks : { spacing : Int } -> List CompiledChunk -> Element Msg
viewChunks { spacing } chunks =
    Element.textColumn [ Element.spacing spacing ] (List.map viewChunk chunks)


bulletIcon : Element msg
bulletIcon =
    Element.el [ Element.paddingEach { top = 0, bottom = 0, left = 20, right = 8 }, Font.bold ]
        (Element.html <|
            let
                radius =
                    3.25

                halfWidth =
                    ceiling radius
            in
            Svg.svg
                [ Svg.Attributes.width (String.fromInt (2 * halfWidth))
                , Svg.Attributes.height (String.fromInt fontSizes.body)
                ]
                [ Svg.circle
                    [ Svg.Attributes.cx (String.fromInt halfWidth)
                    , Svg.Attributes.cy (String.fromFloat (toFloat fontSizes.body - 6.5))
                    , Svg.Attributes.r (String.fromFloat radius)
                    , Svg.Attributes.fill "black"
                    , Svg.Attributes.stroke "none"
                    ]
                    []
                ]
        )


bulletedItem : List CompiledChunk -> Element Msg
bulletedItem chunks =
    Element.row [ widthFill ]
        [ Element.el [ Element.alignTop ] bulletIcon
        , viewChunks { spacing = bulletSpacing } chunks
        ]


viewBullets : List (List CompiledChunk) -> Element Msg
viewBullets bullets =
    Element.column [ Element.spacing bulletSpacing, widthFill ] (List.map bulletedItem bullets)


updateWidget : Int -> Widget -> List CompiledChunk -> List CompiledChunk
updateWidget givenId newWidget chunks =
    List.map
        (\chunk ->
            case chunk of
                Static _ _ ->
                    chunk

                Interactive widgetId currentWidget ->
                    if widgetId == givenId then
                        Interactive widgetId newWidget

                    else
                        chunk

                CompiledBullets bullets ->
                    CompiledBullets (List.map (updateWidget givenId newWidget) bullets)
        )
        chunks


compile : List Chunk -> List CompiledChunk
compile chunks =
    Tuple.first (compileHelp chunks 0 [])


compileHelp : List Chunk -> Int -> List CompiledChunk -> ( List CompiledChunk, Int )
compileHelp chunks widgetId accumulated =
    case chunks of
        first :: rest ->
            let
                prepend compiledChunk =
                    compileHelp rest widgetId (compiledChunk :: accumulated)
            in
            case first of
                Title textFragments ->
                    prepend (Static TitleContext (viewTitle textFragments))

                Section textFragments ->
                    prepend (Static SectionContext (viewSection textFragments))

                Subsection textFragments ->
                    prepend (Static SubsectionContext (viewSubsection textFragments))

                Paragraph textFragments ->
                    prepend (Static ParagraphContext (viewParagraph textFragments))

                CustomBlock widget ->
                    compileHelp rest (widgetId + 1) (Interactive widgetId widget :: accumulated)

                CodeBlock code ->
                    prepend (Static CodeBlockContext (viewCodeBlock code))

                Bullets bullets ->
                    let
                        ( compiledBullets, updatedId ) =
                            compileBullets bullets widgetId []
                    in
                    compileHelp rest updatedId (CompiledBullets compiledBullets :: accumulated)

        [] ->
            ( List.reverse accumulated, widgetId )


compileBullets : List (List Chunk) -> Int -> List (List CompiledChunk) -> ( List (List CompiledChunk), Int )
compileBullets bullets widgetId accumulated =
    case bullets of
        chunks :: rest ->
            let
                ( compiledChunks, updatedId ) =
                    compileHelp chunks widgetId []
            in
            compileBullets rest updatedId (compiledChunks :: accumulated)

        [] ->
            ( List.reverse accumulated, widgetId )


viewTitle : List Text -> Element msg
viewTitle textFragments =
    Element.el
        [ Element.width Element.fill
        , Element.paddingEach { bottom = 8, top = 8, left = 0, right = 0 }
        ]
        (Element.paragraph
            [ Region.heading 1
            , alegreyaSans
            , Font.extraBold
            , Font.size fontSizes.title
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , Border.color lightGrey
            , Element.width Element.fill
            ]
            (renderText TitleContext textFragments)
        )


viewSection : List Text -> Element msg
viewSection textFragments =
    Element.paragraph
        [ Region.heading 2
        , alegreyaSans
        , Font.extraBold
        , Font.size fontSizes.section
        , Element.paddingEach { top = 12, bottom = 0, left = 0, right = 0 }
        ]
        (renderText SectionContext textFragments)


viewSubsection : List Text -> Element msg
viewSubsection textFragments =
    Element.paragraph
        [ Region.heading 3
        , alegreyaSans
        , Font.extraBold
        , Font.size fontSizes.subsection
        , Element.paddingEach { top = 6, bottom = 0, left = 0, right = 0 }
        ]
        (renderText SubsectionContext textFragments)


viewParagraph : List Text -> Element msg
viewParagraph textFragments =
    Element.paragraph [] (renderText ParagraphContext textFragments)


viewCodeBlock : String -> Element msg
viewCodeBlock code =
    Element.column
        [ Border.rounded 5, Element.paddingXY 10 4, sourceCodePro, Background.color lightGrey ]
        (List.map Element.text (String.lines (String.trim code)))


renderText : TextContext -> List Text -> List (Element msg)
renderText context fragments =
    List.map (renderTextFragment context) fragments


inlineCodeElement : Int -> InlineCodeChunk -> Element msg
inlineCodeElement fontSize chunk =
    case chunk of
        Characters characters ->
            Element.text characters

        Spaces spaces ->
            Element.el [ Font.size (round ((2 / 3) * toFloat fontSize)) ] (Element.text spaces)


renderTextFragment : TextContext -> Text -> Element msg
renderTextFragment context fragment =
    case fragment of
        Plain string ->
            Element.text string

        Italic string ->
            Element.el [ Font.italic ] (Element.text string)

        Bold string ->
            Element.el [ Font.bold ] (Element.text string)

        InlineCode chunks ->
            let
                fontSize =
                    case context of
                        TitleContext ->
                            fontSizes.titleCode

                        SectionContext ->
                            fontSizes.sectionCode

                        SubsectionContext ->
                            fontSizes.subsectionCode

                        ParagraphContext ->
                            fontSizes.bodyCode

                        CodeBlockContext ->
                            fontSizes.bodyCode

                backgroundAttributes =
                    if context == ParagraphContext then
                        [ Element.paddingXY 4 2, Border.rounded 3, Background.color lightGrey ]

                    else
                        []
            in
            Element.row
                (sourceCodePro :: Font.size fontSize :: backgroundAttributes)
                (List.map (inlineCodeElement fontSize) chunks)


inlineCodeRegex : Regex
inlineCodeRegex =
    Regex.fromString "(\\s+)|(\\S+)" |> Maybe.withDefault Regex.never


toInlineCodeChunk : Regex.Match -> InlineCodeChunk
toInlineCodeChunk match =
    case match.submatches of
        [ Just spaces, Nothing ] ->
            Spaces (String.repeat (String.length spaces) "\u{00A0}")

        [ Nothing, Just characters ] ->
            Characters characters

        _ ->
            -- Should never happen!
            Spaces ""


toInlineCodeChunks : String -> List InlineCodeChunk
toInlineCodeChunks string =
    let
        matches =
            Regex.find inlineCodeRegex string
    in
    List.map toInlineCodeChunk matches


parseText : List (Inline Never) -> List Text -> Result String (List Text)
parseText inlines accumulated =
    case inlines of
        first :: rest ->
            let
                prepend text =
                    parseText rest (text :: accumulated)
            in
            case first of
                Inline.Text string ->
                    prepend (Plain string)

                Inline.HardLineBreak ->
                    Err "Hard line breaks not yet supported"

                Inline.CodeInline string ->
                    prepend (InlineCode (toInlineCodeChunks string))

                Inline.Link _ _ _ ->
                    Err "Links not yet supported"

                Inline.Image _ _ _ ->
                    Err "Inline images not yet supported"

                Inline.HtmlInline _ _ _ ->
                    Err "Inline custom elements not yet supported"

                Inline.Emphasis 1 [ Inline.Text string ] ->
                    prepend (Italic string)

                Inline.Emphasis 2 [ Inline.Text string ] ->
                    prepend (Bold string)

                Inline.Emphasis _ _ ->
                    Err "Only _italic plain text_ and **bold plain text** are currently supported"

                Inline.Custom _ _ ->
                    Err "Internal error - should not be possible to have a Custom inline"

        [] ->
            Ok (List.reverse accumulated)


parseChunks : Dict String Widget -> List (Block Never Never) -> List Chunk -> Result String (List Chunk)
parseChunks widgets blocks accumulated =
    case blocks of
        first :: rest ->
            let
                prepend chunk =
                    parseChunks widgets rest (chunk :: accumulated)
            in
            case first of
                Block.BlankLine _ ->
                    parseChunks widgets rest accumulated

                Block.ThematicBreak ->
                    Err "ThematicBreak not yet supported"

                Block.Heading _ level inlines ->
                    case level of
                        1 ->
                            Err "Heading level 1 only supported at the beginning of the document"

                        2 ->
                            Result.map Section (parseText inlines []) |> Result.andThen prepend

                        3 ->
                            Result.map Subsection (parseText inlines []) |> Result.andThen prepend

                        _ ->
                            Err ("Heading level " ++ String.fromInt level ++ " not yet supported")

                Block.CodeBlock Block.Indented code ->
                    prepend (CodeBlock code)

                Block.CodeBlock (Block.Fenced _ _) code ->
                    -- TODO support different language types
                    prepend (CodeBlock code)

                Block.Paragraph _ inlines ->
                    Result.andThen (Paragraph >> prepend) (parseText inlines [])

                Block.BlockQuote _ ->
                    Err "Block quotes not yet supported"

                Block.List { type_ } items ->
                    case type_ of
                        Block.Unordered ->
                            Result.andThen (Bullets >> prepend)
                                (Result.combine
                                    (List.map
                                        (\itemBlocks -> parseChunks widgets itemBlocks [])
                                        items
                                    )
                                )

                        Block.Ordered _ ->
                            Err "Numbered lists not yet supported"

                Block.PlainInlines inlines ->
                    case inlines of
                        [ Inline.Image _ _ _ ] ->
                            Err "Images not yet supported"

                        [ Inline.HtmlInline tag [] [] ] ->
                            case Dict.get tag widgets of
                                Just registeredWidget ->
                                    prepend (CustomBlock registeredWidget)

                                Nothing ->
                                    Err ("No widget found with name " ++ tag)

                        [ Inline.HtmlInline tag attributes children ] ->
                            Err "Custom elements with attributes or children not yet supported"

                        [ Inline.Text text ] ->
                            Result.andThen (Paragraph >> prepend) (parseText inlines [])

                        [ inline ] ->
                            Err ("Only images and custom elements supported as standalone elements right now, got " ++ Debug.toString inline)

                        _ ->
                            Result.andThen (Paragraph >> prepend) (parseText inlines [])

                Block.Custom _ _ ->
                    Err "Internal error - should not be possible to have a Custom block"

        [] ->
            Ok (List.reverse accumulated)


parse : List ( String, Widget ) -> String -> Result String Document
parse widgets markdown =
    let
        options =
            { softAsHardLineBreak = False
            , rawHtml = Markdown.Config.ParseUnsafe
            }

        blocks =
            Block.parse (Just options) markdown
    in
    case blocks of
        (Block.Heading _ 1 inlines) :: rest ->
            Result.map2
                (\titleText bodyChunks ->
                    Document
                        { title = Inline.extractText inlines
                        , chunks = compile (Title titleText :: bodyChunks)
                        }
                )
                (parseText inlines [])
                (parseChunks (Dict.fromList widgets) rest [])

        _ ->
            Err "Markdown document must start with a level 1 header"
