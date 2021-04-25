module Render.Elm exposing (paragraphs, renderElement, renderList, renderString)

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
import Parser.Driver
import Parser.Element exposing (Element(..), Mark2Msg)
import Parser.SourceMap exposing (SourceMap)
import Parser.TextCursor
import Render.Types exposing (DisplayMode(..), FRender, RenderArgs, RenderElementDict)
import Render.Utility
import String.Extra
import Utility
import Widget.Data



-- import Widget.Simulations
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

        --case paragraphs str of
        --    [] ->
        --        E.none
        --
        --    [ str_ ] ->
        --        el [] (text str_)
        --
        --    list_ ->
        --        column [ spacing 12 ] (List.map text list_)
        Element name args body sm ->
            renderWithDictionary renderArgs name args body sm

        LX list_ _ ->
            paragraph format (List.map (renderElement renderArgs) list_)


paragraphs : String -> List String
paragraphs str =
    str
        |> String.split "\n\n"
        |> List.map String.trim


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
        , ( "section2", section2 )
        , ( "section3", section3 )
        , ( "list", list )
        , ( "data", dataTable )
        , ( "item", item )
        , ( "link", link )
        , ( "image", image )
        , ( "math", renderMath )
        , ( "m", renderMath )
        , ( "displaymath", renderMathDisplay )
        , ( "dm", renderMathDisplay )
        , ( "center", center )
        , ( "indent", indent )

        -- COMPUTATIONS
        , ( "sum", Widget.Data.sum )
        , ( "average", Widget.Data.average )
        , ( "stdev", Widget.Data.stdev )
        , ( "bargraph", Widget.Data.bargraph )
        , ( "linegraph", Widget.Data.linegraph )
        , ( "scatterplot", Widget.Data.scatterplot )

        -- , ( "gameoflife", Widget.Simulations.gameOfLife )
        ]


getText : Element -> Maybe String
getText element =
    case element of
        LX [ Text content _ ] _ ->
            Just content

        _ ->
            Nothing


getText2 : Element -> String
getText2 element =
    case element of
        LX list_ _ ->
            List.map Render.Utility.extractText list_ |> Maybe.Extra.values |> String.join "\n"

        _ ->
            ""



-- NEW
-- LISTS


listPadding =
    E.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


indentPadding =
    E.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }


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


elementTitle args_ =
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
            el [ Font.bold, Font.size titleSize ] (text title_)


titleSize =
    16


list : FRender Mark2Msg
list renderArgs name args_ body sm =
    case body of
        LX list_ _ ->
            column [ spacing 4, listPadding ]
                (elementTitle args_ :: List.indexedMap (\k item_ -> renderListItem (getPrefixSymbol k args_) renderArgs item_) (filterOutBlankItems list_))

        _ ->
            el [ Font.color redColor ] (text "Malformed list")


dataTable : FRender Mark2Msg
dataTable renderArgs name args_ body sm =
    let
        rawData : List (List String)
        rawData =
            Render.Utility.getCSV body
                |> List.filter (\row -> row /= [ "" ])

        widths : List Float
        widths =
            Render.Utility.columnWidths 10 0 rawData

        headerRow =
            List.member "header" (Utility.entities args_)

        style k =
            if k == 0 && headerRow then
                [ Font.bold, spacing 4 ]

            else
                [ spacing 4 ]

        makeRow : Int -> List Float -> List String -> E.Element Mark2Msg
        makeRow k columnWidths cells =
            row (style k) (List.map2 (\w cell -> el [ E.width (E.px (round w)) ] (text cell)) columnWidths cells)
    in
    column [ spacing 8 ]
        (elementTitle args_ :: List.indexedMap (\k -> makeRow k widths) rawData)


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
            E.row [ E.spacing 8 ] [ prefixSymbol, renderElement renderArgs elt ]

        Element "list" args body _ ->
            case body of
                LX list_ _ ->
                    column [ spacing 4, listPadding ] (elementTitle args :: List.indexedMap (\k item_ -> renderListItem (getPrefixSymbol k args) renderArgs item_) (filterOutBlankItems list_))

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
    column [ Font.size sectionFontSize, paddingAbove (round <| 0.8 * sectionFontSize) ] [ text (getText body |> Maybe.withDefault "no section name found") ]


section2 : FRender Mark2Msg
section2 renderArgs name args body sm =
    column [ Font.size section2FontSize, paddingAbove (round <| 0.8 * section2FontSize) ] [ text (getText body |> Maybe.withDefault "no subsection name found") ]


section3 : FRender Mark2Msg
section3 renderArgs name args body sm =
    column [ Font.size section3FontSize, paddingAbove (round <| 0.8 * section3FontSize) ] [ text (getText body |> Maybe.withDefault "no subsubsection name found") ]


center : FRender Mark2Msg
center renderArgs name args body sm =
    column [ E.centerX ] [ renderElement renderArgs body ]


indent : FRender Mark2Msg
indent renderArgs name args body sm =
    column [ indentPadding ] [ renderElement renderArgs body ]


paddingAbove k =
    E.paddingEach { top = k, bottom = 0, left = 0, right = 0 }


sectionFontSize =
    24


section2FontSize =
    18


section3FontSize =
    16


link : FRender Mark2Msg
link renderArgs name args body sm =
    case Render.Utility.getArg 0 args of
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
                    E.row [ E.centerX ] [ el [ E.width E.fill ] (text c) ]

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
            Render.Utility.getInt 0 args

        g =
            Render.Utility.getInt 1 args

        b =
            Render.Utility.getInt 2 args
    in
    el [ Background.color (E.rgb255 r g b), E.paddingXY 4 2 ] (renderElement renderArgs body)


fontRGB : FRender Mark2Msg
fontRGB renderArgs name args body sm =
    let
        r =
            Render.Utility.getInt 0 args

        g =
            Render.Utility.getInt 1 args

        b =
            Render.Utility.getInt 2 args
    in
    el [ Font.color (E.rgb255 r g b), E.paddingXY 4 2 ] (renderElement renderArgs body)



-- MATH


renderaAsTheoremLikeElement : FRender Mark2Msg
renderaAsTheoremLikeElement renderArgs name args body sm =
    let
        label_ =
            Render.Utility.getArg 0 args

        heading =
            case label_ of
                Nothing ->
                    E.row [] [ el [ Font.bold ] (text <| String.Extra.toSentenceCase name ++ ".") ]

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
-- HELPERS
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
