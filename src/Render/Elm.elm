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
import Parser.Driver
import Parser.Element exposing (Element(..))
import Parser.Getters
import Parser.SourceMap exposing (SourceMap)
import Parser.TextCursor
import String.Extra
import Utility



-- TYPES


type alias RenderArgs =
    { width : Int
    , selectedId : String
    , generation : Int
    , blockOffset : Int
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
                paragraph []
                    [ el [ Font.bold ] (text "[")
                    , el [ Font.color blueColor, Font.bold ] (text (name ++ " "))
                    , el [ Font.color violetColor ] (text (getText body |> Maybe.withDefault ""))
                    , el [ Font.color redColor ] (text " << element misstyped or unimplemented")
                    , el [ Font.bold ] (text "]")
                    ]

        Just f ->
            f renderArgs name args body sm



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
        , ( "section", section )
        , ( "subsection", subsection )
        , ( "list", list )
        , ( "item", item )
        , ( "link", link )
        , ( "image", image )
        , ( "math", renderMath )
        , ( "mathDisplay", renderMathDisplay )
        ]


getText : Element -> Maybe String
getText element =
    case element of
        LX [ Text content _ ] _ ->
            Just content

        _ ->
            Nothing



-- NEW
-- LISTS


listPadding =
    E.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


getPrefixSymbol : List String -> String
getPrefixSymbol args_ =
    let
        option =
            getArg 0 "_xxxx_" args_
    in
    case option of
        "bullet" ->
            "•"

        "none" ->
            ""

        "_xxxx_" ->
            ""

        _ ->
            option


list : FRender Mark2Msg
list renderArgs name args_ body sm =
    case body of
        LX list_ _ ->
            column [ spacing 4, listPadding ] (List.map (renderListItem (getPrefixSymbol args_) renderArgs) list_)

        _ ->
            el [ Font.color redColor ] (text "Malformed list")


renderListItem : String -> RenderArgs -> Element -> E.Element Mark2Msg
renderListItem prefixSymbol renderArgs elt =
    case elt of
        Element "item" _ body _ ->
            let
                prefix =
                    case prefixSymbol of
                        "bullet" ->
                            el [ Font.size 16 ] (text prefixSymbol)

                        _ ->
                            el [ Font.size 16 ] (text prefixSymbol)
            in
            row [ spacing 8 ] [ prefix, renderElement renderArgs elt ]

        Element "list" args body _ ->
            case body of
                LX list_ _ ->
                    column [ spacing 4, listPadding ] (List.map (renderListItem prefixSymbol renderArgs) list_)

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
    18


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
            Utility.keyValueDict args |> Debug.log "DICT"

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
