module Guide.Document exposing (Document, parse, title, view)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Guide.Widget as Widget exposing (Widget)
import Html
import Html.Attributes
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
        , width : Int
        , chunks : List CompiledChunk
        , screenType : ScreenType
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
    | Link { url : String, displayed : Text }
    | Image { url : String, description : String }


type TextContext
    = TitleContext
    | SectionContext
    | SubsectionContext
    | ParagraphContext
    | CodeBlockContext


type ScreenType
    = SmallScreen
    | LargeScreen


type alias ParseConfig =
    { widgets : Dict String Widget
    , screenType : ScreenType
    }


type alias ViewConfig =
    { screenType : ScreenType
    , topLevel : Bool
    }


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


linkBlue : Element.Color
linkBlue =
    Element.rgb255 17 131 204


type alias FontSizes =
    { title : Int
    , section : Int
    , subsection : Int
    , body : Int
    , titleCode : Int
    , sectionCode : Int
    , subsectionCode : Int
    , bodyCode : Int
    , codeBlockCode : Int
    }


largeScreenFontSizes : FontSizes
largeScreenFontSizes =
    { title = 48
    , section = 32
    , subsection = 22
    , body = 14
    , titleCode = 44
    , sectionCode = 30
    , subsectionCode = 22
    , bodyCode = 14
    , codeBlockCode = 14
    }


smallScreenFontSizes : FontSizes
smallScreenFontSizes =
    { title = 32
    , section = 22
    , subsection = 18
    , body = 14
    , titleCode = 28
    , sectionCode = 22
    , subsectionCode = 18
    , bodyCode = 14
    , codeBlockCode = 12
    }


fontSizes : ScreenType -> FontSizes
fontSizes screenType =
    case screenType of
        LargeScreen ->
            largeScreenFontSizes

        SmallScreen ->
            smallScreenFontSizes


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
    let
        fontSize =
            Font.size (fontSizes document.screenType).body

        width =
            Element.width (Element.px document.width)

        padding =
            Element.paddingEach
                { top = 0
                , left = 0
                , right = 0
                , bottom = topLevelSpacing
                }

        mainContent =
            Region.mainContent
    in
    Element.el
        (merriweather :: fontSize :: width :: padding :: mainContent :: attributes)
        (viewChunks { topLevel = True, screenType = document.screenType } document.chunks
            |> Element.map
                (\( id, widget ) ->
                    Document
                        { title = document.title
                        , chunks = updateWidget id widget document.chunks
                        , width = document.width
                        , screenType = document.screenType
                        }
                )
        )


viewChunk : ViewConfig -> CompiledChunk -> Element Msg
viewChunk config chunk =
    case chunk of
        Static textContext element ->
            Element.map never element

        Interactive id widget ->
            Widget.view (\newWidget -> ( id, newWidget )) widget

        CompiledBullets bullets ->
            viewBullets config bullets


viewChunks : { topLevel : Bool, screenType : ScreenType } -> List CompiledChunk -> Element Msg
viewChunks config chunks =
    let
        spacing =
            if config.topLevel then
                topLevelSpacing

            else
                bulletSpacing
    in
    Element.textColumn [ Element.spacing spacing, widthFill ]
        (List.map (viewChunk config) chunks)


bulletIcon : ViewConfig -> Element msg
bulletIcon { screenType, topLevel } =
    let
        radius =
            case screenType of
                LargeScreen ->
                    3.25

                SmallScreen ->
                    2.75

        height =
            case screenType of
                LargeScreen ->
                    6.5

                SmallScreen ->
                    6.5

        halfWidth =
            ceiling radius

        rightPadding =
            case screenType of
                LargeScreen ->
                    8

                SmallScreen ->
                    6

        leftPadding =
            if topLevel then
                case screenType of
                    LargeScreen ->
                        20

                    SmallScreen ->
                        12

            else
                rightPadding
    in
    Element.el
        [ Element.paddingEach
            { top = 0
            , bottom = 0
            , left = leftPadding
            , right = rightPadding
            }
        , Font.bold
        ]
        (Element.html <|
            Svg.svg
                [ Svg.Attributes.width (String.fromInt (2 * halfWidth))
                , Svg.Attributes.height (String.fromInt (fontSizes screenType).body)
                ]
                [ Svg.circle
                    [ Svg.Attributes.cx (String.fromInt halfWidth)
                    , Svg.Attributes.cy (String.fromFloat (toFloat (fontSizes screenType).body - height))
                    , Svg.Attributes.r (String.fromFloat radius)
                    , Svg.Attributes.fill "black"
                    , Svg.Attributes.stroke "none"
                    ]
                    []
                ]
        )


bulletedItem : ViewConfig -> List CompiledChunk -> Element Msg
bulletedItem config chunks =
    Element.row [ widthFill ]
        [ Element.el [ Element.alignTop ] (bulletIcon config)
        , viewChunks { config | topLevel = False } chunks
        ]


viewBullets : ViewConfig -> List (List CompiledChunk) -> Element Msg
viewBullets config bullets =
    Element.column [ Element.spacing bulletSpacing, widthFill ]
        (List.map (bulletedItem config) bullets)


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


compile : ScreenType -> List Chunk -> List CompiledChunk
compile screenType chunks =
    Tuple.first (compileHelp screenType chunks 0 [])


compileHelp : ScreenType -> List Chunk -> Int -> List CompiledChunk -> ( List CompiledChunk, Int )
compileHelp screenType chunks widgetId accumulated =
    case chunks of
        first :: rest ->
            let
                prepend compiledChunk =
                    compileHelp screenType rest widgetId (compiledChunk :: accumulated)
            in
            case first of
                Title textFragments ->
                    prepend (Static TitleContext (viewTitle screenType textFragments))

                Section textFragments ->
                    prepend (Static SectionContext (viewSection screenType textFragments))

                Subsection textFragments ->
                    prepend (Static SubsectionContext (viewSubsection screenType textFragments))

                Paragraph textFragments ->
                    prepend (Static ParagraphContext (viewParagraph screenType textFragments))

                CustomBlock widget ->
                    compileHelp screenType
                        rest
                        (widgetId + 1)
                        (Interactive widgetId widget :: accumulated)

                CodeBlock code ->
                    prepend (Static CodeBlockContext (viewCodeBlock screenType code))

                Bullets bullets ->
                    let
                        ( compiledBullets, updatedId ) =
                            compileBullets screenType bullets widgetId []
                    in
                    compileHelp screenType
                        rest
                        updatedId
                        (CompiledBullets compiledBullets :: accumulated)

        [] ->
            ( List.reverse accumulated, widgetId )


compileBullets : ScreenType -> List (List Chunk) -> Int -> List (List CompiledChunk) -> ( List (List CompiledChunk), Int )
compileBullets screenType bullets widgetId accumulated =
    case bullets of
        chunks :: rest ->
            let
                ( compiledChunks, updatedId ) =
                    compileHelp screenType chunks widgetId []
            in
            compileBullets screenType rest updatedId (compiledChunks :: accumulated)

        [] ->
            ( List.reverse accumulated, widgetId )


viewTitle : ScreenType -> List Text -> Element msg
viewTitle screenType textFragments =
    Element.el
        [ Element.width Element.fill
        , Element.paddingEach { bottom = 8, top = 8, left = 0, right = 0 }
        ]
        (Element.paragraph
            [ Region.heading 1
            , alegreyaSans
            , Font.extraBold
            , Font.size (fontSizes screenType).title
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , Border.color lightGrey
            , Element.width Element.fill
            ]
            (renderText screenType TitleContext textFragments)
        )


viewSection : ScreenType -> List Text -> Element msg
viewSection screenType textFragments =
    Element.paragraph
        [ Region.heading 2
        , alegreyaSans
        , Font.extraBold
        , Font.size (fontSizes screenType).section
        , Element.paddingEach { top = 12, bottom = 0, left = 0, right = 0 }
        ]
        (renderText screenType SectionContext textFragments)


viewSubsection : ScreenType -> List Text -> Element msg
viewSubsection screenType textFragments =
    Element.paragraph
        [ Region.heading 3
        , alegreyaSans
        , Font.extraBold
        , Font.size (fontSizes screenType).subsection
        , Element.paddingEach { top = 6, bottom = 0, left = 0, right = 0 }
        ]
        (renderText screenType SubsectionContext textFragments)


viewParagraph : ScreenType -> List Text -> Element msg
viewParagraph screenType textFragments =
    Element.paragraph [] (renderText screenType ParagraphContext textFragments)


viewCodeBlockLine : String -> Element msg
viewCodeBlockLine line =
    if String.isEmpty line then
        Element.text "\n"

    else
        Element.text line


viewCodeBlock : ScreenType -> String -> Element msg
viewCodeBlock screenType code =
    Element.column
        [ Border.rounded 5
        , Element.paddingXY 12 10
        , sourceCodePro
        , Background.color lightGrey
        , Font.size (fontSizes screenType).codeBlockCode
        , Element.scrollbarX
        ]
        (List.map viewCodeBlockLine (String.lines (String.trim code)))


renderText : ScreenType -> TextContext -> List Text -> List (Element msg)
renderText screenType context fragments =
    List.map (renderTextFragment screenType context) fragments


inlineCodeElement : Int -> InlineCodeChunk -> Element msg
inlineCodeElement fontSize chunk =
    case chunk of
        Characters characters ->
            Element.text characters

        Spaces spaces ->
            Element.el [ Font.size (round ((2 / 3) * toFloat fontSize)) ] (Element.text spaces)


inlineCodeChunkToString : InlineCodeChunk -> String
inlineCodeChunkToString chunk =
    case chunk of
        Spaces string ->
            string

        Characters string ->
            string


toPlainText : Text -> String
toPlainText textFragment =
    case textFragment of
        Plain string ->
            string

        Italic string ->
            string

        Bold string ->
            string

        Link { displayed } ->
            toPlainText displayed

        InlineCode chunks ->
            String.concat (List.map inlineCodeChunkToString chunks)

        Image { description } ->
            description


codeFontSize : ScreenType -> TextContext -> Int
codeFontSize screenType context =
    case context of
        TitleContext ->
            (fontSizes screenType).titleCode

        SectionContext ->
            (fontSizes screenType).sectionCode

        SubsectionContext ->
            (fontSizes screenType).subsectionCode

        ParagraphContext ->
            (fontSizes screenType).bodyCode

        CodeBlockContext ->
            (fontSizes screenType).codeBlockCode


codeBackgroundAttributes : List (Element.Attribute msg)
codeBackgroundAttributes =
    [ Element.paddingXY 4 2, Border.rounded 3, Background.color lightGrey ]


renderTextFragment : ScreenType -> TextContext -> Text -> Element msg
renderTextFragment screenType context fragment =
    case fragment of
        Plain string ->
            Element.text string

        Italic string ->
            Element.el [ Font.italic ] (Element.text string)

        Bold string ->
            Element.el [ Font.bold ] (Element.text string)

        Link { url, displayed } ->
            let
                attributes =
                    case displayed of
                        Plain _ ->
                            []

                        Italic string ->
                            [ Font.italic ]

                        Bold string ->
                            [ Font.bold ]

                        InlineCode chunks ->
                            sourceCodePro
                                :: Font.size (codeFontSize screenType context)
                                :: codeBackgroundAttributes

                        Image _ ->
                            -- Should never happen
                            []

                        Link _ ->
                            -- Should never happen
                            []

                labelText =
                    toPlainText displayed
            in
            Element.link (Font.color linkBlue :: Font.underline :: attributes)
                { url = url, label = Element.text labelText }

        InlineCode chunks ->
            let
                fontSize =
                    codeFontSize screenType context

                backgroundAttributes =
                    if context == ParagraphContext then
                        codeBackgroundAttributes

                    else
                        []
            in
            Element.row
                (sourceCodePro :: Font.size fontSize :: backgroundAttributes)
                (List.map (inlineCodeElement fontSize) chunks)

        Image { url, description } ->
            Element.html <|
                Html.img
                    [ Html.Attributes.src url
                    , Html.Attributes.alt description
                    , Html.Attributes.style "max-width" "100%"
                    , Html.Attributes.style "max-height" "100%"
                    ]
                    []



--Element.image [] { src = url, description = description }


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

                Inline.Link url _ urlInlines ->
                    let
                        prependLink displayed =
                            prepend (Link { url = url, displayed = displayed })
                    in
                    case urlInlines of
                        [ Inline.Text string ] ->
                            prependLink (Plain string)

                        [ Inline.Emphasis 1 [ Inline.Text string ] ] ->
                            prependLink (Italic string)

                        [ Inline.Emphasis 2 [ Inline.Text string ] ] ->
                            prependLink (Bold string)

                        [ Inline.CodeInline string ] ->
                            prependLink (InlineCode [ Characters string ])

                        _ ->
                            Err ("Link label must currently be a single plain text, italic, bold or inline code fragment, got " ++ Debug.toString urlInlines)

                Inline.Image url _ imageInlines ->
                    prepend (Image { url = url, description = Inline.extractText imageInlines })

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


parseChunks : ParseConfig -> List (Block Never Never) -> List Chunk -> Result String (List Chunk)
parseChunks config blocks accumulated =
    case blocks of
        first :: rest ->
            let
                prepend chunk =
                    parseChunks config rest (chunk :: accumulated)
            in
            case first of
                Block.BlankLine _ ->
                    parseChunks config rest accumulated

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
                                        (\itemBlocks -> parseChunks config itemBlocks [])
                                        items
                                    )
                                )

                        Block.Ordered _ ->
                            Err "Numbered lists not yet supported"

                Block.PlainInlines inlines ->
                    case inlines of
                        [ Inline.HtmlInline tag [] [] ] ->
                            case Dict.get tag config.widgets of
                                Just registeredWidget ->
                                    prepend (CustomBlock registeredWidget)

                                Nothing ->
                                    Err ("No widget found with name " ++ tag)

                        [ Inline.HtmlInline tag attributes children ] ->
                            Err "Custom elements with attributes or children not yet supported"

                        _ ->
                            Result.andThen (Paragraph >> prepend) (parseText inlines [])

                Block.Custom _ _ ->
                    Err "Internal error - should not be possible to have a Custom block"

        [] ->
            Ok (List.reverse accumulated)


parse : { screenWidth : Int, widgets : List ( String, Widget ) } -> String -> Result String Document
parse { screenWidth, widgets } markdown =
    let
        options =
            { softAsHardLineBreak = False
            , rawHtml = Markdown.Config.ParseUnsafe
            }

        blocks =
            Block.parse (Just options) markdown

        width =
            min (screenWidth - 24) 640

        screenType =
            if screenWidth > 600 then
                LargeScreen

            else
                SmallScreen
    in
    case blocks of
        (Block.Heading _ 1 inlines) :: rest ->
            Result.map2
                (\titleText bodyChunks ->
                    Document
                        { title = Inline.extractText inlines
                        , chunks = compile screenType (Title titleText :: bodyChunks)
                        , width = width
                        , screenType = screenType
                        }
                )
                (parseText inlines [])
                (parseChunks { screenType = screenType, widgets = Dict.fromList widgets } rest [])

        _ ->
            Err "Markdown document must start with a level 1 header"
