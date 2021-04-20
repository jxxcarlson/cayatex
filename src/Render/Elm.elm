module Render.Elm exposing (Mark2Msg(..), renderElement, renderList, renderString)

import Dict exposing (Dict)
import Element as E exposing (column, el, paragraph, px, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import List.Extra
import Maybe.Extra
import Parser.Data
import Parser.Driver
import Parser.Element exposing (Element(..))
import Parser.Getters
import Parser.SourceMap exposing (SourceMap)
import Parser.TextCursor
import Render.Utility
import String.Extra
import Utility



-- TYPES


type alias RenderArgs =
    { width : Int
    , selectedId : String
    , generation : Int
    , blockOffset : Int
    , parserData : Parser.Data.Data
    }


type alias FRender a =
    RenderArgs -> String -> List String -> Element -> Maybe SourceMap -> E.Element a


type alias RenderElementDict a =
    Dict String (FRender a)


type Mark2Msg
    = Mark2Msg


type DisplayMode
    = InlineMathMode
    | DisplayMathMode



-- STYLE PARAMETERS


textWidth =
    E.width (E.px 200)


format =
    []



-- TOP-LEVEL RENDERERS


renderString : RenderArgs -> String -> E.Element Mark2Msg
renderString renderArgs str =
    Parser.Driver.parseLoop renderArgs.generation renderArgs.blockOffset str
        |> Parser.TextCursor.parseResult
        |> renderList renderArgs


renderList : RenderArgs -> List Element -> E.Element Mark2Msg
renderList renderArgs list_ =
    paragraph format (List.map (renderElement renderArgs) list_)


renderElement : RenderArgs -> Element -> E.Element Mark2Msg
renderElement renderArgs element =
    case element of
        Text str _ ->
            -- TODO
            el [] (text str)

        Element name args body sm ->
            renderWithDictionary renderArgs name args body sm

        LX list_ _ ->
            paragraph format (List.map (renderElement renderArgs) list_)


theoremLikeElements =
    [ "theorem", "proposition", "proof", "definition", "example", "problem", "corollary", "lemma" ]


renderWithDictionary renderArgs name args body sm =
    case Dict.get name renderElementDict of
        Nothing ->
            if List.member name theoremLikeElements then
                renderaAsTheoremLikeElement renderArgs name args body sm

            else
                renderMissingElement name body

        Just f ->
            f renderArgs name args body sm


renderMissingElement : String -> Element -> E.Element Mark2Msg
renderMissingElement name body =
    paragraph []
        [ el [ Font.bold ] (text "[")
        , el [ Font.color blueColor, Font.bold ] (text (name ++ " "))
        , el [ Font.color violetColor ] (text (getText body |> Maybe.withDefault ""))
        , el [ Font.color redColor ] (text " << element misstyped or unimplemented")
        , el [ Font.bold ] (text "]")
        ]



-- RENDERER DICTIONARY


renderElementDict : RenderElementDict Mark2Msg
renderElementDict =
    Dict.fromList
        [ ( "Error", error )
        , ( "strong", renderStrong )
        , ( "italic", renderItalic )
        , ( "highlight", highlight )
        , ( "highlightRGB", highlightRGB )
        , ( "fontRGB", fontRGB )
        , ( "code", renderCode )
        , ( "codeblock", renderCodeBlock )
        , ( "poetry", poetry )
        , ( "section", section )
        , ( "subsection", subsection )
        , ( "list", list )
        , ( "item", item )
        , ( "link", link )
        , ( "image", image )
        , ( "math", renderMath )
        , ( "mathdisplay", renderMathDisplay )

        -- COMPUTATIONS
        , ( "sum", sum )
        , ( "average", average )
        , ( "stdev", stdev )
        ]


getText : Element -> Maybe String
getText element =
    case element of
        LX [ Text content _ ] _ ->
            Just content

        _ ->
            Nothing


getTextList : Element -> List String
getTextList element =
    case element of
        LX list_ _ ->
            List.map extractText list_
                |> List.map (Maybe.withDefault "")
                |> List.map (String.split ",")
                |> List.map (List.map String.trim)
                |> List.concat

        --  |> Maybe.Extra.values
        _ ->
            []


getText2 : Element -> String
getText2 element =
    case element of
        LX list_ _ ->
            List.map extractText list_ |> Maybe.Extra.values |> String.join "\n"

        _ ->
            ""



--case element of
--    LX list_ _ ->
--        List.map extractText list_ |> Maybe.Extra.values |> String.join "\n"
--
--    _ ->
--        ""


extractText : Element -> Maybe String
extractText element =
    case element of
        Text content _ ->
            Just content

        _ ->
            Nothing



-- NEW
-- LISTS


listPadding =
    E.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


getPrefixSymbol : Int -> List String -> E.Element Mark2Msg
getPrefixSymbol k args_ =
    case List.head (Utility.entities args_) |> Maybe.map String.trim of
        Just "numbered" ->
            el [ Font.size 12, E.alignTop, E.moveDown 2.2 ] (text (String.fromInt (k + 1) ++ "."))

        Nothing ->
            el [ Font.size 16 ] (text "•")

        Just "bullet" ->
            el [ Font.size 16 ] (text "•")

        Just "none" ->
            E.none

        Just str ->
            el [ Font.size 16 ] (text str)


listTitle args_ =
    let
        dict =
            Utility.keyValueDict args_

        title =
            Dict.get "title" dict
    in
    case title of
        Nothing ->
            E.none

        Just title_ ->
            el [ Font.bold ] (text title_)


list : FRender Mark2Msg
list renderArgs name args_ body sm =
    case body of
        LX list_ _ ->
            column [ spacing 4, listPadding ]
                (listTitle args_ :: List.indexedMap (\k item_ -> renderListItem (getPrefixSymbol k args_) renderArgs item_) (filterOutBlankItems list_))

        _ ->
            el [ Font.color redColor ] (text "Malformed list")


filterOutBlankItems : List Element -> List Element
filterOutBlankItems list_ =
    List.filter (\item_ -> not (isBlankItem item_)) list_


isBlankItem : Element -> Bool
isBlankItem el =
    case el of
        Text str _ ->
            String.trim str == ""

        _ ->
            False


renderListItem : E.Element Mark2Msg -> RenderArgs -> Element -> E.Element Mark2Msg
renderListItem prefixSymbol renderArgs elt =
    case elt of
        Element "item" _ body _ ->
            row [ spacing 8 ] [ prefixSymbol, renderElement renderArgs elt ]

        Element "list" args body _ ->
            case body of
                LX list_ _ ->
                    column [ spacing 4, listPadding ] (listTitle args :: List.indexedMap (\k item_ -> renderListItem (getPrefixSymbol k args) renderArgs item_) (filterOutBlankItems list_))

                _ ->
                    el [ Font.color redColor ] (text "Malformed list")

        _ ->
            E.none


item : FRender Mark2Msg
item renderArgs name args_ body sm =
    paragraph [] [ renderElement renderArgs body ]


error : FRender Mark2Msg
error renderArgs name args_ body sm =
    el [ Font.color violetColor ] (renderElement renderArgs body)



-- ELEMENTARY RENDERERS
-- CODE


renderCode : FRender Mark2Msg
renderCode renderArgs _ _ body sm =
    let
        adjustedBody =
            getText body
                |> Maybe.withDefault "(body)"
                |> String.replace "\\[" "["
                |> String.replace "\\]" "]"
                |> (\text -> Text text sm)
    in
    el
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.size 14
        , Font.color codeColor
        ]
        (renderElement renderArgs adjustedBody)


getLXText : Element -> String
getLXText element =
    case element of
        LX list_ _ ->
            List.map (getText >> Maybe.withDefault "") list_ |> String.join "\n"

        _ ->
            ""


poetry : FRender Mark2Msg
poetry renderArgs _ _ body sm =
    column
        [ Font.size 14
        , Render.Utility.htmlAttribute "white-space" "pre"
        , indentation
        , spacing 4
        ]
        (List.map text (getLines (getText2 body |> String.trim)))


renderCodeBlock : FRender Mark2Msg
renderCodeBlock renderArgs _ _ body sm =
    column
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.size 14
        , Font.color codeColor
        , Render.Utility.htmlAttribute "white-space" "pre"
        , indentation
        ]
        (List.map text (getLines (getText2 body |> String.trim)))


indentation =
    E.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


getLines : String -> List String
getLines str =
    let
        nonBreakingSpace =
            String.fromChar '\u{00A0}'
    in
    str
        |> String.lines
        |> List.map
            (\s ->
                if s == "" then
                    nonBreakingSpace

                else
                    String.replace " " nonBreakingSpace s
            )



-- STYLE ELEMENTS
-- NEW


section : FRender Mark2Msg
section renderArgs name args body sm =
    paragraph [ Font.size sectionFontSize ] [ text (getText body |> Maybe.withDefault "no section name found") ]


subsection : FRender Mark2Msg
subsection renderArgs name args body sm =
    paragraph [ Font.size subsectionFontSize ] [ text (getText body |> Maybe.withDefault "no subsection name found") ]


sectionFontSize =
    22


subsectionFontSize =
    16


link : FRender Mark2Msg
link renderArgs name args body sm =
    case getArg_ 0 args of
        Nothing ->
            let
                url_ =
                    getText body |> Maybe.withDefault "missing url"
            in
            E.newTabLink []
                { url = url_
                , label = el [ Font.color linkColor, Font.italic ] (text url_)
                }

        Just labelText ->
            E.newTabLink []
                { url = getText body |> Maybe.withDefault "missing url"
                , label = el [ Font.color linkColor, Font.italic ] (text labelText)
                }


image : FRender Mark2Msg
image renderArgs name args body sm =
    let
        dict =
            Utility.keyValueDict args

        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        caption =
            case Dict.get "caption" dict of
                Nothing ->
                    E.none

                Just c ->
                    row [ E.centerX ] [ el [ E.width E.fill ] (text c) ]

        width =
            case Dict.get "width" dict of
                Nothing ->
                    E.fill

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            E.fill

                        Just w ->
                            E.px w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    E.centerX

                Just "left" ->
                    E.alignLeft

                Just "right" ->
                    E.alignRight

                Just "center" ->
                    E.centerX

                _ ->
                    E.centerX
    in
    E.column [ spacing 8, placement, E.width (E.px 400) ]
        [ E.image [ E.width width, placement ]
            { src = getText body |> Maybe.withDefault "no image url", description = description }
        , caption
        ]


renderStrong : FRender Mark2Msg
renderStrong renderArgs _ _ body sm =
    el [ Font.bold ] (renderElement renderArgs body)


renderItalic : FRender Mark2Msg
renderItalic renderArgs _ _ body sm =
    el [ Font.italic ] (renderElement renderArgs body)


highlight : FRender Mark2Msg
highlight renderArgs _ _ body _ =
    el [ Background.color yellowColor, E.paddingXY 4 2 ] (renderElement renderArgs body)


highlightRGB : FRender Mark2Msg
highlightRGB renderArgs _ args body sm =
    let
        r =
            getInt 0 args

        g =
            getInt 1 args

        b =
            getInt 2 args
    in
    el [ Background.color (E.rgb255 r g b), E.paddingXY 4 2 ] (renderElement renderArgs body)


fontRGB : FRender Mark2Msg
fontRGB renderArgs name args body sm =
    let
        r =
            getInt 0 args

        g =
            getInt 1 args

        b =
            getInt 2 args
    in
    el [ Font.color (E.rgb255 r g b), E.paddingXY 4 2 ] (renderElement renderArgs body)



-- MATH


renderaAsTheoremLikeElement : FRender Mark2Msg
renderaAsTheoremLikeElement renderArgs name args body sm =
    let
        label_ =
            getArg_ 0 args

        heading =
            case label_ of
                Nothing ->
                    row [] [ el [ Font.bold ] (text <| String.Extra.toSentenceCase name ++ ".") ]

                Just label ->
                    paragraph []
                        [ el [ Font.bold ] (text <| String.Extra.toSentenceCase name)
                        , el [] (text <| " (" ++ label ++ ")")
                        , el [ Font.bold ] (text <| ".")
                        ]
    in
    column [ spacing 3 ]
        [ heading
        , el [] (renderElement renderArgs body)
        ]


renderMathDisplay : FRender Mark2Msg
renderMathDisplay rendArgs name args body sm =
    case getText body of
        Just content ->
            mathText rendArgs DisplayMathMode content sm

        Nothing ->
            el [ Font.color redColor ] (text "Error rendering math !!!")


renderMath : FRender Mark2Msg
renderMath renderArgs name args body sm =
    case getText body of
        Just content ->
            mathText renderArgs InlineMathMode content sm

        Nothing ->
            el [ Font.color redColor ] (text "Error rendering math !!!")


mathText : RenderArgs -> DisplayMode -> String -> Maybe SourceMap -> E.Element Mark2Msg
mathText renderArgs displayMode content sm =
    Html.Keyed.node "span"
        []
        [ ( String.fromInt renderArgs.generation, mathText_ displayMode renderArgs.selectedId content sm )
        ]
        |> E.html


mathText_ : DisplayMode -> String -> String -> Maybe SourceMap -> Html Mark2Msg
mathText_ displayMode selectedId content sm =
    Html.node "math-text"
        -- active sm selectedId  ++
        [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string content)

        -- , clicker sm
        -- , HA.id (makeId sm)
        ]
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True



-- MATH


sum : FRender Mark2Msg
sum renderArgs name args body sm =
    let
        numbers_ =
            getTextList body

        numbers =
            List.map String.toFloat numbers_ |> Maybe.Extra.values

        sum_ =
            List.sum numbers

        precision =
            getPrecisionWithDefault 2 args
    in
    row [ spacing 8 ] (text "sum" :: List.map text numbers_ ++ [ text "=" ] ++ [ text (String.fromFloat (roundTo precision sum_)) ])


average : FRender Mark2Msg
average renderArgs name args body sm =
    let
        numbers_ =
            getTextList body

        numbers =
            List.map String.toFloat numbers_ |> Maybe.Extra.values

        n =
            toFloat (List.length numbers)

        sum_ =
            List.sum numbers

        average_ =
            sum_ / n

        precision =
            getPrecisionWithDefault 2 args
    in
    row [ spacing 8 ] (text "average" :: List.map text numbers_ ++ [ text "=" ] ++ [ text (String.fromFloat (roundTo precision average_)) ])


stdev : FRender Mark2Msg
stdev renderArgs name args body sm =
    let
        numbers_ =
            getTextList body

        numbers =
            List.map String.toFloat numbers_ |> Maybe.Extra.values

        n =
            toFloat (List.length numbers)

        sum_ =
            List.sum numbers

        average_ =
            sum_ / n

        deltas =
            List.map (\x -> x - average_) numbers

        sumOfDeltasSquared =
            List.map2 (*) deltas deltas |> List.sum

        stdev_ =
            sqrt sumOfDeltasSquared / (n - 1)

        precision =
            getPrecisionWithDefault 2 args
    in
    row [ spacing 8 ] (text "stdev" :: List.map text numbers_ ++ [ text "=" ] ++ [ text (String.fromFloat (roundTo precision stdev_)) ])


getPrecisionWithDefault : Int -> List String -> Int
getPrecisionWithDefault default args =
    getPrecision args |> Maybe.withDefault default


getPrecision : List String -> Maybe Int
getPrecision args =
    let
        dict =
            Utility.keyValueDict args
    in
    Dict.get "precision" dict |> Maybe.andThen String.toInt


roundTo : Int -> Float -> Float
roundTo k x =
    let
        factor =
            10.0 ^ toFloat k
    in
    toFloat (round (factor * x)) / factor



-- HELPERS


getInt : Int -> List String -> Int
getInt k stringList =
    List.Extra.getAt k stringList
        |> Maybe.andThen String.toInt
        |> Maybe.withDefault 0


getArg_ : Int -> List String -> Maybe String
getArg_ k stringList =
    List.Extra.getAt k stringList


getArg : Int -> String -> List String -> String
getArg k default stringList =
    List.Extra.getAt k stringList |> Maybe.withDefault default



-- active : SourceMap -> String -> List (Attribute LaTeXMsg)
-- active sm selectedId =
--     let
--         id_ =
--             makeId sm
--     in
--     [ clicker sm, HA.id id_, highlight "#FAA" selectedId id_ ]
-- clicker sm =
--     onClick (SendSourceMap sm)
-- COLORS


linkColor =
    E.rgb 0 0 0.8


blackColor =
    E.rgb 0 0 0


redColor =
    E.rgb 0.7 0 0


blueColor =
    E.rgb 0 0 0.8


yellowColor =
    E.rgb 1.0 1.0 0


violetColor =
    E.rgb 0.4 0 0.8


codeColor =
    -- E.rgb 0.2 0.5 1.0
    E.rgb 0.4 0 0.8
