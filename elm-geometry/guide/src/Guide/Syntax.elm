module Guide.Syntax exposing (Chunk(..), ChunkType(..), parse)

import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Set


type ChunkType
    = Comment
    | String
    | Number
    | Character
    | Keyword
    | Symbol
    | Whitespace
    | LocalIdentifier
    | TypeOrModule
    | ModuleMember { moduleName : String, memberName : String }


type Chunk
    = Chunk ChunkType String


parse : String -> Result String (List (List Chunk))
parse code =
    Parser.run parser (normalize code)
        |> Result.map splitLines
        |> Result.mapError toMessage


toMessage : List Parser.DeadEnd -> String
toMessage deadEnds =
    case deadEnds of
        first :: _ ->
            String.concat
                [ "Syntax parsing error at row "
                , String.fromInt first.row
                , ", column "
                , String.fromInt first.col
                ]

        [] ->
            "Syntax parsing error"


parser : Parser (List Chunk)
parser =
    Parser.loop [] parseHelp


parseHelp : List Chunk -> Parser (Parser.Step (List Chunk) (List Chunk))
parseHelp accumulated =
    Parser.oneOf
        [ Parser.succeed (\parsedChunk -> Parser.Loop (parsedChunk :: accumulated))
            |= chunk
        , Parser.succeed (\() -> Parser.Done (List.reverse accumulated))
            |= Parser.end
        ]


normalize : String -> String
normalize code =
    code |> removeCarriageReturns |> addTrailingNewline


removeCarriageReturns : String -> String
removeCarriageReturns code =
    String.replace "\u{000D}" "" code


addTrailingNewline : String -> String
addTrailingNewline code =
    if String.endsWith "\n" code then
        code

    else
        code ++ "\n"


chunk : Parser Chunk
chunk =
    Parser.oneOf
        [ Parser.map (Chunk Whitespace) whitespace
        , Parser.map (Chunk Comment) comment
        , Parser.map (Chunk String) string
        , Parser.map (Chunk Character) character
        , Parser.map (Chunk Keyword) keyword
        , Parser.map (Chunk Symbol) symbol
        , Parser.map classifyIdentifier identifier
        , Parser.map (Chunk Number) number
        ]


classifyIdentifier : String -> Chunk
classifyIdentifier fullString =
    Chunk (identifierType fullString) fullString


identifierType : String -> ChunkType
identifierType fullString =
    let
        parts =
            String.split "." fullString |> List.filter (not << String.isEmpty)
    in
    case List.Extra.splitWhen isNotCapitalized parts of
        Nothing ->
            -- No parts start with a lower-case letter
            TypeOrModule

        Just ( [], _ ) ->
            -- All parts start with a lower-case letter
            LocalIdentifier

        Just ( _, [] ) ->
            -- Should never happen (should get Nothing instead), but logically would correspond to a
            -- module
            TypeOrModule

        Just ( moduleParts, memberName :: _ ) ->
            -- Some parts start with an upper-case letter (module name parts), then further parts
            -- start with a lower-case letter (member name parts - usually just one but might be
            -- more if the module member is a record)
            ModuleMember { moduleName = String.join "." moduleParts, memberName = memberName }


isNotCapitalized : String -> Bool
isNotCapitalized str =
    let
        firstCharacter =
            String.left 1 str
    in
    String.toLower firstCharacter == firstCharacter


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\n'


whitespace : Parser String
whitespace =
    Parser.getChompedString (Parser.chompIf isWhitespace |. Parser.chompWhile isWhitespace)


comment : Parser String
comment =
    Parser.oneOf [ between "{-" "-}", between "--" "\n" ]


string : Parser String
string =
    between "\"" "\""


character : Parser String
character =
    between "'" "'"


between : String -> String -> Parser String
between start end =
    Parser.getChompedString (Parser.symbol start |. Parser.chompUntil end |. Parser.symbol end)


number : Parser String
number =
    let
        bareNumber =
            Parser.number
                { int = Just (always ())
                , float = Just (always ())
                , hex = Nothing
                , octal = Nothing
                , binary = Nothing
                }
    in
    Parser.getChompedString <|
        Parser.oneOf
            [ Parser.token "-" |. bareNumber
            , bareNumber
            ]


keyword : Parser String
keyword =
    Parser.getChompedString <|
        Parser.oneOf <|
            List.map Parser.keyword
                [ "let"
                , "in"
                , "if"
                , "then"
                , "else"
                , "case"
                , "of"
                , "where"
                , "module"
                , "import"
                , "exposing"
                , "type"
                , "alias"
                , "as"
                , "infix"
                , "infixl"
                , "infixr"
                , "port"
                , "effect"
                , "command"
                , "subscription"
                ]


symbol : Parser String
symbol =
    Parser.getChompedString <|
        Parser.oneOf <|
            List.map Parser.token
                [ "->"
                , "["
                , "]"
                , ","
                , "::"
                , "++"
                , "&&"
                , "||"
                , "|>"
                , "<|"
                , ">>"
                , "<<"
                , "|."
                , "|="
                , ">="
                , "<="
                , "<"
                , ">"
                , "=="
                , "/="
                , "="
                , "\\"
                , "//"
                , "^"
                , "+"
                , "-"
                , "/"
                , "*"
                , "("
                , ")"
                , "{"
                , "}"
                , ":"
                ]


identifier : Parser String
identifier =
    Parser.getChompedString <|
        Parser.variable
            { start = \char -> Char.isAlpha char || char == '_'
            , inner = \char -> Char.isAlphaNum char || char == '.'
            , reserved = Set.empty
            }


splitLines : List Chunk -> List (List Chunk)
splitLines chunks =
    splitLinesHelp [] [] chunks


splitLinesHelp : List (List Chunk) -> List Chunk -> List Chunk -> List (List Chunk)
splitLinesHelp accumulatedLines accumulatedChunks chunks =
    case chunks of
        firstChunk :: remainingChunks ->
            let
                (Chunk chunkType chunkText) =
                    firstChunk
            in
            case String.lines chunkText of
                [] ->
                    splitLinesHelp accumulatedLines accumulatedChunks remainingChunks

                [ fragmentNotAtEndOfLine ] ->
                    if String.isEmpty fragmentNotAtEndOfLine then
                        splitLinesHelp accumulatedLines accumulatedChunks remainingChunks

                    else
                        splitLinesHelp
                            accumulatedLines
                            (Chunk chunkType fragmentNotAtEndOfLine :: accumulatedChunks)
                            remainingChunks

                fragmentAtEndOfLine :: followingFragments ->
                    let
                        newLine =
                            List.reverse (Chunk chunkType fragmentAtEndOfLine :: accumulatedChunks)
                    in
                    splitLinesHelp
                        (newLine :: accumulatedLines)
                        []
                        (Chunk chunkType (String.join "\n" followingFragments) :: remainingChunks)

        [] ->
            if List.isEmpty accumulatedChunks then
                List.reverse accumulatedLines

            else
                List.reverse (List.reverse accumulatedChunks :: accumulatedLines)
