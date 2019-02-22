module Guide.Page exposing
    ( Page
    , readme, with
    , sourceUrl, displayedUrl, title, widgets
    , matching
    )

{-|

@docs Page

@docs readme, with

@docs sourceUrl, displayedUrl, title, widgets

@docs matching

-}

import Dict exposing (Dict)
import Guide.Document as Document exposing (Document)
import Guide.Widget as Widget exposing (Widget)
import Http
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>))
import Url.Parser.Query


type Page
    = Readme { url : String }
    | Page { title : String, widgets : Dict String Widget }


readme : { url : String } -> Page
readme =
    Readme


with : { title : String, widgets : List ( String, Widget ) } -> Page
with given =
    Page { title = given.title, widgets = Dict.fromList given.widgets }


sourceUrl : List String -> Page -> String
sourceUrl rootPath page =
    case page of
        Readme { url } ->
            url

        Page properties ->
            Url.Builder.absolute (rootPath ++ [ "pages", properties.title ++ ".md" ]) []


displayedUrl : List String -> Page -> Maybe String -> String
displayedUrl rootPath page maybeFragment =
    case page of
        Readme { url } ->
            Url.Builder.custom Url.Builder.Absolute
                rootPath
                [ Url.Builder.string "page" "README" ]
                maybeFragment

        Page properties ->
            Url.Builder.custom Url.Builder.Absolute
                rootPath
                [ Url.Builder.string "page" properties.title ]
                maybeFragment


title : Page -> String
title page =
    case page of
        Readme _ ->
            "README"

        Page properties ->
            properties.title


widgets : Page -> Dict String Widget
widgets page =
    case page of
        Readme _ ->
            Dict.empty

        Page properties ->
            properties.widgets


type alias Route =
    { query : Maybe String
    , fragment : Maybe String
    }


urlParser : List String -> Url.Parser.Parser (Route -> a) a
urlParser rootPath =
    let
        rootParser =
            List.map Url.Parser.s rootPath
                |> List.foldl (\segment current -> current </> segment) Url.Parser.top
    in
    Url.Parser.map Route <|
        rootParser
            </> Url.Parser.query (Url.Parser.Query.string "page")
            </> Url.Parser.fragment identity


matchesQuery : Maybe String -> Page -> Bool
matchesQuery query page =
    case ( query, page ) of
        ( Nothing, Readme _ ) ->
            True

        ( Just queryValue, _ ) ->
            queryValue == title page

        _ ->
            False


matching : { url : Url, rootPath : List String } -> List Page -> Maybe { page : Page, fragment : Maybe String }
matching { url, rootPath } pages =
    case Url.Parser.parse (urlParser rootPath) url of
        Just { query, fragment } ->
            case List.filter (matchesQuery query) pages of
                [ page ] ->
                    Just { page = page, fragment = fragment }

                _ ->
                    Nothing

        Nothing ->
            Nothing
