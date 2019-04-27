module Guide.Document exposing (Config, Document, Msg(..), parse, view)

import BoundingBox2d
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Geometry.Svg as Svg
import Guide.Color as Color
import Guide.Screen as Screen
import Guide.Syntax as Syntax
import Guide.Text as Text
import Guide.Widget as Widget exposing (Widget)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Markdown.Block as Block exposing (Block)
import Markdown.Config
import Markdown.Inline as Inline exposing (Inline)
import Regex exposing (Regex)
import Result.Extra as Result
import Set exposing (Set)
import Svg
import Svg.Attributes
import Url.Builder


type Document
    = Document
        { titleFragments : List Text
        , chunks : List (Chunk Int)
        }


type InternalMsg
    = InternalWidgetUpdated Int Widget
    | InternalImageLoaded String


type Msg
    = Updated Document
    | ImageLoaded String


type Chunk widgetId
    = Section (List Text)
    | Subsection (List Text)
    | Paragraph (List Text)
    | CustomBlock widgetId Widget
    | PlainCodeBlock String
    | ElmCodeBlock (List (List Syntax.Chunk))
    | Bullets (List (List (Chunk widgetId)))


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


type alias Config =
    { screenClass : Screen.Class
    , moduleNames : Set String
    , rootUrl : String
    , lastModified : Maybe String
    , author : String
    , packageName : String
    }


type Level
    = TopLevel
    | WithinBulletedList


widthFill : Element.Attribute msg
widthFill =
    Element.width Element.fill


topLevelSpacing : Int
topLevelSpacing =
    16


bulletSpacing : Int
bulletSpacing =
    10


view : Config -> List (Element.Attribute Msg) -> Document -> Element Msg
view config givenAttributes (Document document) =
    let
        fontFamily =
            Text.fontFamily Text.Body

        fontSize =
            Font.size (Text.fontSize config.screenClass Text.Body Text.Prose)

        mainContent =
            Region.mainContent

        bottomSpacer =
            Element.el [ Element.height (Element.px 16) ] Element.none

        toMsg internalMessage =
            case internalMessage of
                InternalWidgetUpdated id widget ->
                    Updated <|
                        Document { document | chunks = updateWidget id widget document.chunks }

                InternalImageLoaded url ->
                    ImageLoaded url
    in
    Element.column
        (fontFamily :: fontSize :: mainContent :: givenAttributes)
        [ viewTitle config document.titleFragments |> Element.map toMsg
        , spacer 20
        , viewChunks config TopLevel document.chunks |> Element.map toMsg
        , bottomSpacer
        ]


viewChunk : Config -> Level -> Chunk Int -> Element InternalMsg
viewChunk config level chunk =
    case chunk of
        Section textFragments ->
            viewSection config textFragments

        Subsection textFragments ->
            viewSubsection config textFragments

        Paragraph textFragments ->
            viewParagraph config textFragments

        CustomBlock id widget ->
            Widget.view (\newWidget -> InternalWidgetUpdated id newWidget) widget

        PlainCodeBlock contents ->
            viewPlainCodeBlock config contents

        ElmCodeBlock lines ->
            viewElmCodeBlock config lines

        Bullets bullets ->
            viewBullets config level bullets


viewChunks : Config -> Level -> List (Chunk Int) -> Element InternalMsg
viewChunks config level chunks =
    Element.textColumn [ widthFill ] (viewChunksHelp config level chunks [])


viewChunksHelp : Config -> Level -> List (Chunk Int) -> List (Element InternalMsg) -> List (Element InternalMsg)
viewChunksHelp config level chunks accumulated =
    case chunks of
        [] ->
            List.reverse accumulated

        [ last ] ->
            List.reverse (viewChunk config level last :: accumulated)

        chunk :: next :: remaining ->
            viewChunksHelp
                config
                level
                (next :: remaining)
                (spacer (interChunkSpacing level chunk next)
                    :: viewChunk config level chunk
                    :: accumulated
                )


minSpacingAbove : Chunk a -> Int
minSpacingAbove chunk =
    case chunk of
        Section _ ->
            24

        Subsection _ ->
            20

        Paragraph _ ->
            8

        CustomBlock _ _ ->
            8

        PlainCodeBlock _ ->
            12

        ElmCodeBlock _ ->
            12

        Bullets _ ->
            16


minSpacingBelow : Chunk a -> Int
minSpacingBelow chunk =
    case chunk of
        Section _ ->
            16

        Subsection _ ->
            12

        Paragraph _ ->
            8

        CustomBlock _ _ ->
            8

        PlainCodeBlock _ ->
            12

        ElmCodeBlock _ ->
            12

        Bullets _ ->
            16


interChunkSpacing : Level -> Chunk a -> Chunk a -> Int
interChunkSpacing level first second =
    case level of
        WithinBulletedList ->
            bulletSpacing

        TopLevel ->
            max (minSpacingBelow first) (minSpacingAbove second)


bulletIcon : Config -> Level -> Element msg
bulletIcon { screenClass } level =
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
            case level of
                TopLevel ->
                    case screenClass of
                        Screen.Large ->
                            20

                        Screen.Small ->
                            12

                WithinBulletedList ->
                    rightPadding
    in
    Element.el
        [ Element.paddingEach
            { top = 0
            , bottom = 0
            , left = leftPadding
            , right = rightPadding
            }
        ]
        (let
            bodyFontSize =
                Text.fontSize screenClass Text.Body Text.Prose
         in
         Element.html <|
            Svg.svg
                [ Svg.Attributes.width (String.fromInt (2 * halfWidth))
                , Svg.Attributes.height (String.fromInt bodyFontSize)
                ]
                [ Svg.circle
                    [ Svg.Attributes.cx (String.fromInt halfWidth)
                    , Svg.Attributes.cy (String.fromFloat (toFloat bodyFontSize - height))
                    , Svg.Attributes.r (String.fromFloat radius)
                    , Svg.Attributes.fill "black"
                    , Svg.Attributes.stroke "none"
                    ]
                    []
                ]
        )


bulletedItem : Config -> Level -> List (Chunk Int) -> Element InternalMsg
bulletedItem config level chunks =
    Element.row [ widthFill ]
        [ Element.el [ Element.alignTop ] (bulletIcon config level)
        , viewChunks config WithinBulletedList chunks
        ]


viewBullets : Config -> Level -> List (List (Chunk Int)) -> Element InternalMsg
viewBullets config level bullets =
    Element.column [ Element.spacing bulletSpacing, widthFill ]
        (List.map (bulletedItem config level) bullets)


updateWidget : Int -> Widget -> List (Chunk Int) -> List (Chunk Int)
updateWidget givenId newWidget chunks =
    List.map
        (\chunk ->
            case chunk of
                Section _ ->
                    chunk

                Subsection _ ->
                    chunk

                Paragraph _ ->
                    chunk

                CustomBlock id _ ->
                    if id == givenId then
                        CustomBlock id newWidget

                    else
                        chunk

                PlainCodeBlock _ ->
                    chunk

                ElmCodeBlock _ ->
                    chunk

                Bullets bullets ->
                    Bullets (List.map (updateWidget givenId newWidget) bullets)
        )
        chunks


spacer : Int -> Element msg
spacer height =
    Element.el [ Element.height (Element.px height) ] Element.none


hamburgerIcon : Element msg
hamburgerIcon =
    let
        width =
            20

        height =
            toFloat (Text.fontSize Screen.Small Text.Title Text.Prose)

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


viewTitle : Config -> List Text -> Element InternalMsg
viewTitle config textFragments =
    let
        titleElement =
            Element.el [ Element.width Element.fill ]
                (Element.row [ Element.width Element.fill ]
                    [ Element.paragraph
                        [ Region.heading 1
                        , Text.fontFamily Text.Title
                        , Font.size (Text.fontSize config.screenClass Text.Title Text.Prose)
                        , Text.lineSpacing config.screenClass Text.Title
                        , Element.width Element.fill
                        ]
                        (renderText config Text.Title textFragments)
                    , case config.screenClass of
                        Screen.Small ->
                            Element.link
                                [ Element.alignTop
                                , Font.size 32
                                , Element.paddingEach { left = 8, top = 0, bottom = 0, right = 0 }
                                ]
                                { url = config.rootUrl
                                , label = hamburgerIcon
                                }

                        Screen.Large ->
                            Element.none
                    ]
                )
    in
    Element.el [ widthFill ] <|
        case config.lastModified of
            Just date ->
                Element.column [ Element.width Element.fill ]
                    [ titleElement
                    , spacer 8
                    , Element.paragraph
                        [ Font.color Color.lastModified
                        , Font.italic
                        , Font.size (Text.fontSize config.screenClass Text.Body Text.Prose)
                        ]
                        [ Element.text "Last updated on ", Element.text date ]
                    ]

            Nothing ->
                titleElement


toId : List Text -> String
toId textFragments =
    String.concat (List.map toPlainText textFragments)
        |> String.toLower
        |> String.replace " " "-"


viewSection : Config -> List Text -> Element InternalMsg
viewSection config textFragments =
    Element.paragraph
        [ Region.heading 2
        , Text.fontFamily Text.Section
        , Font.size (Text.fontSize config.screenClass Text.Section Text.Prose)
        , Text.lineSpacing config.screenClass Text.Section
        , Element.htmlAttribute (Html.Attributes.id (toId textFragments))
        ]
        (renderText config Text.Section textFragments)


viewSubsection : Config -> List Text -> Element InternalMsg
viewSubsection config textFragments =
    Element.paragraph
        [ Region.heading 3
        , Text.fontFamily Text.Subsection
        , Font.size (Text.fontSize config.screenClass Text.Subsection Text.Prose)
        , Text.lineSpacing config.screenClass Text.Subsection
        , Element.htmlAttribute (Html.Attributes.id (toId textFragments))
        ]
        (renderText config Text.Subsection textFragments)


viewParagraph : Config -> List Text -> Element InternalMsg
viewParagraph config textFragments =
    Element.paragraph [ Text.lineSpacing config.screenClass Text.Body ]
        (renderText config Text.Body textFragments)


codeBlockAttributes : Config -> List (Element.Attribute msg)
codeBlockAttributes { screenClass } =
    [ Border.rounded 5
    , Element.paddingXY 16 12
    , Text.codeFontFamily
    , Background.color Color.codeBlockBackground
    , Font.size (Text.fontSize screenClass Text.CodeBlock Text.Code)
    , Text.lineSpacing screenClass Text.CodeBlock
    , Element.scrollbarX
    ]


viewElmCodeBlock : Config -> List (List Syntax.Chunk) -> Element msg
viewElmCodeBlock config syntaxChunks =
    Element.column (codeBlockAttributes config) (List.map (viewCodeBlockLine config) syntaxChunks)


viewPlainCodeBlock : Config -> String -> Element msg
viewPlainCodeBlock config code =
    Element.column (codeBlockAttributes config) [ Element.text code ]


viewCodeBlockLine : Config -> List Syntax.Chunk -> Element msg
viewCodeBlockLine config chunks =
    case chunks of
        [] ->
            Element.el [] (Element.text "\n")

        [ Syntax.Chunk Syntax.Whitespace _ ] ->
            Element.el [] (Element.text "\n")

        _ ->
            Element.row [] (List.map (viewCodeChunk config) chunks)


viewCodeChunk : Config -> Syntax.Chunk -> Element msg
viewCodeChunk config chunk =
    case chunk of
        Syntax.Chunk Syntax.Comment string ->
            viewColoredChunk Color.comment string

        Syntax.Chunk Syntax.String string ->
            viewColoredChunk Color.string string

        Syntax.Chunk Syntax.Number string ->
            viewColoredChunk Color.number string

        Syntax.Chunk Syntax.Character string ->
            viewColoredChunk Color.string string

        Syntax.Chunk Syntax.Keyword string ->
            viewColoredChunk Color.keyword string

        Syntax.Chunk Syntax.TypeOrModule name ->
            viewModule config name

        Syntax.Chunk (Syntax.ModuleMember moduleMember) string ->
            viewModuleMember config moduleMember string

        Syntax.Chunk Syntax.LocalIdentifier string ->
            viewColoredChunk Color.black string

        Syntax.Chunk Syntax.Symbol string ->
            viewColoredChunk Color.symbol string

        Syntax.Chunk Syntax.Whitespace string ->
            viewColoredChunk Color.white string


viewColoredChunk : Element.Color -> String -> Element msg
viewColoredChunk color text =
    Element.el [ Font.color color ] (Element.text text)


documentationLink : Config -> String -> Maybe String -> String -> Element msg
documentationLink config moduleName memberName fullString =
    Element.link [ Font.color Color.linkText ]
        { url =
            Url.Builder.custom
                (Url.Builder.CrossOrigin "https://package.elm-lang.org")
                [ "packages"
                , config.author
                , config.packageName
                , "latest"
                , String.replace "." "-" moduleName
                ]
                []
                memberName
        , label = Element.text fullString
        }


viewModule : Config -> String -> Element msg
viewModule config name =
    if Set.member name config.moduleNames then
        documentationLink config name Nothing name

    else
        viewColoredChunk Color.black name


viewModuleMember : Config -> { moduleName : String, memberName : String } -> String -> Element msg
viewModuleMember config { moduleName, memberName } fullString =
    if Set.member moduleName config.moduleNames then
        documentationLink config moduleName (Just memberName) fullString

    else
        viewColoredChunk Color.black fullString


renderText : Config -> Text.Location -> List Text -> List (Element InternalMsg)
renderText config textLocation fragments =
    List.map (renderTextFragment config textLocation) fragments


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


inlineCodeBackgroundAttributes : List (Element.Attribute msg)
inlineCodeBackgroundAttributes =
    [ Element.paddingXY 6 4, Border.rounded 3, Background.color Color.inlineCodeBackground ]


renderImage : { url : String, description : String } -> Element InternalMsg
renderImage { url, description } =
    Element.html <|
        Html.img
            [ Html.Attributes.src url
            , Html.Attributes.alt description
            , Html.Attributes.style "max-width" "100%"
            , Html.Attributes.style "max-height" "100%"
            , Html.Events.on "load" (Decode.succeed (InternalImageLoaded url))
            ]
            []


renderTextFragment : Config -> Text.Location -> Text -> Element InternalMsg
renderTextFragment config textLocation fragment =
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
                            ( Text.codeFontFamily
                                :: Font.size (Text.fontSize config.screenClass textLocation Text.Code)
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
                codeFontSize =
                    Text.fontSize config.screenClass textLocation Text.Code
            in
            Element.row
                (Text.codeFontFamily :: Font.size codeFontSize :: inlineCodeBackgroundAttributes)
                (List.map (inlineCodeElement codeFontSize) chunks)

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


parseChunks : Dict String Widget -> List (Block Never Never) -> List (Chunk ()) -> Result String (List (Chunk ()))
parseChunks widgets blocks accumulated =
    case blocks of
        first :: rest ->
            let
                prepend chunk =
                    parseChunks widgets rest (chunk :: accumulated)

                parseCode code =
                    case Syntax.parse code of
                        Ok lines ->
                            prepend (ElmCodeBlock lines)

                        Err message ->
                            Err message
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
                    parseCode code

                Block.CodeBlock (Block.Fenced _ fence) code ->
                    case fence.language of
                        Just "elm" ->
                            parseCode code

                        Just "text" ->
                            prepend (PlainCodeBlock code)

                        Just language ->
                            Err ("Language " ++ language ++ " not recognized")

                        Nothing ->
                            prepend (PlainCodeBlock code)

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
                        [ Inline.HtmlInline tag [] [] ] ->
                            case Dict.get tag widgets of
                                Just registeredWidget ->
                                    prepend (CustomBlock () registeredWidget)

                                Nothing ->
                                    Err ("No widget found with name " ++ tag)

                        [ Inline.HtmlInline _ _ _ ] ->
                            Err "Custom elements with attributes or children not yet supported"

                        _ ->
                            Result.andThen (Paragraph >> prepend) (parseText inlines [])

                Block.Custom _ _ ->
                    Err "Internal error - should not be possible to have a Custom block"

        [] ->
            Ok (List.reverse accumulated)


tagChunk : Chunk () -> Int -> ( Chunk Int, Int )
tagChunk chunk nextId =
    case chunk of
        Section textFragments ->
            ( Section textFragments, nextId )

        Subsection textFragments ->
            ( Subsection textFragments, nextId )

        Paragraph textFragments ->
            ( Paragraph textFragments, nextId )

        CustomBlock () widget ->
            ( CustomBlock nextId widget, nextId + 1 )

        PlainCodeBlock code ->
            ( PlainCodeBlock code, nextId )

        ElmCodeBlock syntaxChunks ->
            ( ElmCodeBlock syntaxChunks, nextId )

        Bullets bullets ->
            let
                ( taggedBullets, updatedId ) =
                    tagBullets bullets nextId
            in
            ( Bullets taggedBullets, updatedId )


tagChunks : List (Chunk ()) -> Int -> ( List (Chunk Int), Int )
tagChunks chunks nextId =
    case chunks of
        first :: rest ->
            let
                ( taggedRest, updatedId ) =
                    tagChunks rest nextId

                ( taggedFirst, finalId ) =
                    tagChunk first updatedId
            in
            ( taggedFirst :: taggedRest, finalId )

        [] ->
            ( [], nextId )


tagBullets : List (List (Chunk ())) -> Int -> ( List (List (Chunk Int)), Int )
tagBullets bullets nextId =
    case bullets of
        first :: rest ->
            let
                ( taggedRest, updatedId ) =
                    tagBullets rest nextId

                ( taggedFirst, finalId ) =
                    tagChunks first updatedId
            in
            ( taggedFirst :: taggedRest, finalId )

        [] ->
            ( [], nextId )


parse : Dict String Widget -> String -> Result String ( Document, Set String )
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
                    ( Document
                        { titleFragments = titleText
                        , chunks = Tuple.first (tagChunks bodyChunks 1)
                        }
                    , collectImageUrls blocks
                    )
                )
                (parseText inlines [])
                (parseChunks widgets rest [])

        _ ->
            Err "Markdown document must start with a level 1 header"


collectImageUrls : List (Block Never Never) -> Set String
collectImageUrls blocks =
    List.map (Block.queryInlines getImageUrls) blocks
        |> List.concat
        |> Set.fromList


getImageUrls : Inline i -> List String
getImageUrls inline =
    case inline of
        Inline.Image url _ _ ->
            [ url ]

        _ ->
            []
