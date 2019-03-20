module Guide.Document exposing (Document, parse, title, view)

import BoundingBox2d
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Geometry.Svg as Svg
import Guide.Color as Color
import Guide.Font as Font
import Guide.Screen as Screen
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
        , chunks : List CompiledChunk
        , screenClass : Screen.Class
        }


type alias Msg =
    ( Int, Widget )


type CompiledChunk
    = Static TextContext (Element Never)
    | Interactive Int Widget
    | CompiledBullets (List (List CompiledChunk))


type Chunk
    = Title (List Text) String
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


type alias ParseConfig =
    { widgets : Dict String Widget
    , screenClass : Screen.Class
    }


type alias ViewConfig =
    { screenClass : Screen.Class
    , topLevel : Bool
    }


widthFill : Element.Attribute msg
widthFill =
    Element.width Element.fill


topLevelSpacing : Int
topLevelSpacing =
    12


bulletSpacing : Int
bulletSpacing =
    8


maxWidth : Int
maxWidth =
    640


gutterPadding : Int
gutterPadding =
    12


title : Document -> String
title (Document document) =
    document.title


view : List (Element.Attribute Document) -> Document -> Element Document
view attributes (Document document) =
    let
        fontSize =
            Font.size (Font.sizes document.screenClass).body

        width =
            Element.width (Element.fill |> Element.maximum maxWidth)

        padding =
            Element.padding gutterPadding

        mainContent =
            Region.mainContent
    in
    Element.el
        (Font.merriweather :: fontSize :: width :: padding :: mainContent :: attributes)
        (viewChunks { topLevel = True, screenClass = document.screenClass } document.chunks
            |> Element.map
                (\( id, widget ) ->
                    Document
                        { title = document.title
                        , chunks = updateWidget id widget document.chunks
                        , screenClass = document.screenClass
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


viewChunks : ViewConfig -> List CompiledChunk -> Element Msg
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
bulletIcon { screenClass, topLevel } =
    let
        radius =
            case screenClass of
                Screen.Large ->
                    3.25

                Screen.Small ->
                    2.75

        height =
            case screenClass of
                Screen.Large ->
                    6.5

                Screen.Small ->
                    6.5

        halfWidth =
            ceiling radius

        rightPadding =
            case screenClass of
                Screen.Large ->
                    8

                Screen.Small ->
                    6

        leftPadding =
            if topLevel then
                case screenClass of
                    Screen.Large ->
                        20

                    Screen.Small ->
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
                , Svg.Attributes.height (String.fromInt (Font.sizes screenClass).body)
                ]
                [ Svg.circle
                    [ Svg.Attributes.cx (String.fromInt halfWidth)
                    , Svg.Attributes.cy (String.fromFloat (toFloat (Font.sizes screenClass).body - height))
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


compile : Screen.Class -> List Chunk -> List CompiledChunk
compile screenClass chunks =
    Tuple.first (compileHelp screenClass chunks 0 [])


compileHelp : Screen.Class -> List Chunk -> Int -> List CompiledChunk -> ( List CompiledChunk, Int )
compileHelp screenClass chunks widgetId accumulated =
    case chunks of
        first :: rest ->
            let
                prepend compiledChunk =
                    compileHelp screenClass rest widgetId (compiledChunk :: accumulated)
            in
            case first of
                Title textFragments rootUrl ->
                    prepend (Static TitleContext (viewTitle screenClass textFragments rootUrl))

                Section textFragments ->
                    prepend (Static SectionContext (viewSection screenClass textFragments))

                Subsection textFragments ->
                    prepend (Static SubsectionContext (viewSubsection screenClass textFragments))

                Paragraph textFragments ->
                    prepend (Static ParagraphContext (viewParagraph screenClass textFragments))

                CustomBlock widget ->
                    compileHelp screenClass
                        rest
                        (widgetId + 1)
                        (Interactive widgetId widget :: accumulated)

                CodeBlock code ->
                    prepend (Static CodeBlockContext (viewCodeBlock screenClass code))

                Bullets bullets ->
                    let
                        ( compiledBullets, updatedId ) =
                            compileBullets screenClass bullets widgetId []
                    in
                    compileHelp screenClass
                        rest
                        updatedId
                        (CompiledBullets compiledBullets :: accumulated)

        [] ->
            ( List.reverse accumulated, widgetId )


compileBullets : Screen.Class -> List (List Chunk) -> Int -> List (List CompiledChunk) -> ( List (List CompiledChunk), Int )
compileBullets screenClass bullets widgetId accumulated =
    case bullets of
        chunks :: rest ->
            let
                ( compiledChunks, updatedId ) =
                    compileHelp screenClass chunks widgetId []
            in
            compileBullets screenClass rest updatedId (compiledChunks :: accumulated)

        [] ->
            ( List.reverse accumulated, widgetId )


hamburgerIcon : Element msg
hamburgerIcon =
    let
        width =
            20

        height =
            toFloat (Font.sizes Screen.Small).title

        rectangle bottomY topY =
            Svg.boundingBox2d [] <|
                BoundingBox2d.fromExtrema
                    { minX = 2
                    , maxX = 18
                    , minY = height - topY
                    , maxY = height - bottomY
                    }
    in
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width (String.fromFloat width)
            , Svg.Attributes.height (String.fromFloat height)
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.fill "black"
            ]
            [ rectangle 7 9
            , rectangle 13 15
            , rectangle 19 21
            ]


viewTitle : Screen.Class -> List Text -> String -> Element msg
viewTitle screenClass textFragments rootUrl =
    Element.el
        [ Element.width Element.fill
        , Element.paddingEach { bottom = 8, top = 0, left = 0, right = 0 }
        ]
        (Element.row
            [ Element.width Element.fill
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , Element.paddingEach { top = 0, bottom = 8, left = 0, right = 0 }
            , Border.color Color.dividerLine
            ]
            [ Element.paragraph
                [ Region.heading 1
                , Font.alegreyaSans
                , Font.extraBold
                , Font.size (Font.sizes screenClass).title
                , Element.width Element.fill
                ]
                (renderText screenClass TitleContext textFragments)
            , Element.link [ Element.alignTop, Font.size 32 ]
                { url = rootUrl
                , label = hamburgerIcon
                }
            ]
        )


viewSection : Screen.Class -> List Text -> Element msg
viewSection screenClass textFragments =
    Element.paragraph
        [ Region.heading 2
        , Font.alegreyaSans
        , Font.extraBold
        , Font.size (Font.sizes screenClass).section
        , Element.paddingEach { top = 12, bottom = 0, left = 0, right = 0 }
        ]
        (renderText screenClass SectionContext textFragments)


viewSubsection : Screen.Class -> List Text -> Element msg
viewSubsection screenClass textFragments =
    Element.paragraph
        [ Region.heading 3
        , Font.alegreyaSans
        , Font.extraBold
        , Font.size (Font.sizes screenClass).subsection
        , Element.paddingEach { top = 6, bottom = 0, left = 0, right = 0 }
        ]
        (renderText screenClass SubsectionContext textFragments)


viewParagraph : Screen.Class -> List Text -> Element msg
viewParagraph screenClass textFragments =
    Element.paragraph [] (renderText screenClass ParagraphContext textFragments)


viewCodeBlockLine : String -> Element msg
viewCodeBlockLine line =
    if String.isEmpty line then
        Element.text "\n"

    else
        Element.text line


viewCodeBlock : Screen.Class -> String -> Element msg
viewCodeBlock screenClass code =
    Element.column
        [ Border.rounded 5
        , Element.paddingXY 12 10
        , Font.sourceCodePro
        , Background.color Color.codeBlockBackground
        , Font.size (Font.sizes screenClass).codeBlockCode
        , Element.scrollbarX
        ]
        (List.map viewCodeBlockLine (String.lines (String.trim code)))


renderText : Screen.Class -> TextContext -> List Text -> List (Element msg)
renderText screenClass context fragments =
    List.map (renderTextFragment screenClass context) fragments


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


codeFontSize : Screen.Class -> TextContext -> Int
codeFontSize screenClass context =
    let
        fontSizes =
            Font.sizes screenClass
    in
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
            fontSizes.codeBlockCode


inlineCodeBackgroundAttributes : List (Element.Attribute msg)
inlineCodeBackgroundAttributes =
    [ Element.paddingXY 4 2, Border.rounded 3, Background.color Color.inlineCodeBackground ]


renderImage : { url : String, description : String } -> Element msg
renderImage { url, description } =
    Element.html <|
        Html.img
            [ Html.Attributes.src url
            , Html.Attributes.alt description
            , Html.Attributes.style "max-width" "100%"
            , Html.Attributes.style "max-height" "100%"
            ]
            []


renderTextFragment : Screen.Class -> TextContext -> Text -> Element msg
renderTextFragment screenClass context fragment =
    case fragment of
        Plain string ->
            Element.text string

        Italic string ->
            Element.el [ Font.italic ] (Element.text string)

        Bold string ->
            Element.el [ Font.bold ] (Element.text string)

        Link { url, displayed } ->
            let
                ( attributes, label ) =
                    case displayed of
                        Plain string ->
                            ( [], Element.text string )

                        Italic string ->
                            ( [ Font.italic ], Element.text string )

                        Bold string ->
                            ( [ Font.bold ], Element.text string )

                        InlineCode chunks ->
                            ( Font.sourceCodePro
                                :: Font.size (codeFontSize screenClass context)
                                :: inlineCodeBackgroundAttributes
                            , Element.text <|
                                String.concat (List.map inlineCodeChunkToString chunks)
                            )

                        Image properties ->
                            ( [], renderImage properties )

                        Link _ ->
                            -- Should never happen
                            ( [], Element.none )
            in
            Element.link (Font.color Color.linkText :: attributes) { url = url, label = label }

        InlineCode chunks ->
            let
                fontSize =
                    codeFontSize screenClass context

                backgroundAttributes =
                    if context == ParagraphContext then
                        inlineCodeBackgroundAttributes

                    else
                        []
            in
            Element.row
                (Font.sourceCodePro :: Font.size fontSize :: backgroundAttributes)
                (List.map (inlineCodeElement fontSize) chunks)

        Image properties ->
            renderImage properties


inlineCodeRegex : Regex
inlineCodeRegex =
    Regex.fromString "(\\s+)|(\\S+)" |> Maybe.withDefault Regex.never


nonBreakingSpace : String
nonBreakingSpace =
    "\u{00A0}"


toInlineCodeChunk : Regex.Match -> InlineCodeChunk
toInlineCodeChunk match =
    case match.submatches of
        [ Just spaces, Nothing ] ->
            Spaces (String.repeat (String.length spaces) nonBreakingSpace)

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

                        [ Inline.Image imageUrl _ imageInlines ] ->
                            prependLink <|
                                Image
                                    { url = imageUrl
                                    , description = Inline.extractText imageInlines
                                    }

                        _ ->
                            Err "Link label must currently be a single plain text, italic, bold or inline code fragment, or an image"

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


parse : { screenClass : Screen.Class, widgets : List ( String, Widget ), rootUrl : String } -> String -> Result String Document
parse { screenClass, widgets, rootUrl } markdown =
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
                        , chunks = compile screenClass (Title titleText rootUrl :: bodyChunks)
                        , screenClass = screenClass
                        }
                )
                (parseText inlines [])
                (parseChunks { screenClass = screenClass, widgets = Dict.fromList widgets } rest [])

        _ ->
            Err "Markdown document must start with a level 1 header"
