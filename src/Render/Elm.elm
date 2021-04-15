module Render.Elm exposing (Mark2Msg(..), renderElement, renderList, renderString)

import Dict exposing (Dict)
import Element exposing (Element, column, el, paragraph, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import List.Extra
import Parser.Driver
import Parser.Element
import Parser.Getters
import Parser.SourceMap
import Parser.TextCursor



-- TYPES


type alias FRender a =
    Int -> Int -> String -> List String -> Parser.Element.Element -> Maybe Parser.SourceMap.SourceMap -> Element a


type RenderFunction a
    = I (FRender a)
    | B (FRender a)


type alias RenderElementDict a =
    Dict String (RenderFunction a)


type Mark2Msg
    = Mark2Msg


type DisplayMode
    = InlineMathMode
    | DisplayMathMode



-- STYLE PARAMETERS


textWidth =
    Element.width (Element.px 200)


format =
    []



-- TOP-LEVEL RENDERERS


renderString : Int -> Int -> String -> Element Mark2Msg
renderString generation blockOffset str =
    Parser.Driver.parseLoop generation blockOffset str
        |> Parser.TextCursor.parseResult
        |> renderList generation blockOffset


renderList : Int -> Int -> List Parser.Element.Element -> Element Mark2Msg
renderList generation blockOffset list =
    paragraph format (List.map (renderElement generation blockOffset) list)


renderElement : Int -> Int -> Parser.Element.Element -> Element Mark2Msg
renderElement generation blockOffset element =
    case element of
        Parser.Element.Text str _ ->
            el [] (text str)

        Parser.Element.Element name args body sm ->
            renderWithDictionary generation blockOffset name args body sm

        Parser.Element.LX list _ ->
            paragraph format (List.map (renderElement generation blockOffset) list)



-- RENDERER DICTIONARY


renderWithDictionary generation blockOffset name args body sm =
    case Dict.get name renderElementDict of
        Nothing ->
            paragraph []
                [ el [ Font.bold ] (text "[")
                , el [ Font.color blueColor, Font.bold ] (text (name ++ " "))
                , el [ Font.color violetColor ] (text (getText body |> Maybe.withDefault ""))
                , el [ Font.color redColor ] (text " is misstyped or unimplemented")
                , el [ Font.bold ] (text "]")
                ]

        Just f ->
            case f of
                I g ->
                    g generation blockOffset name args body sm

                B g ->
                    g generation blockOffset name args body sm


renderElementDict : RenderElementDict Mark2Msg
renderElementDict =
    Dict.fromList
        [ ( "strong", I renderStrong )
        , ( "italic", I renderItalic )
        , ( "highlight", I highlight )
        , ( "highlightRGB", I highlightRGB )
        , ( "fontRGB", I fontRGB )
        , ( "code", I renderCode )
        , ( "link", I link )
        , ( "math", I renderMath )
        , ( "mathDisplay", B renderMathDisplay )
        , ( "theorem", B renderTheorem )
        ]


getText : Parser.Element.Element -> Maybe String
getText element =
    case element of
        Parser.Element.LX [ Parser.Element.Text content _ ] _ ->
            Just content

        _ ->
            Nothing



-- ELEMENTARY RENDERERS
-- CODE


renderCode : FRender Mark2Msg
renderCode generation blockOffset _ _ body sm =
    let
        adjustedBody =
            getText body
                |> Maybe.withDefault "(body)"
                |> String.replace "\\[" "["
                |> String.replace "\\]" "]"
                |> (\text -> Parser.Element.Text text sm)
    in
    el
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.size 14
        , Font.color codeColor
        ]
        (renderElement generation blockOffset adjustedBody)



-- STYLE ELEMENTS
-- NEW


link : FRender Mark2Msg
link generation blockOffset name args body sm =
    Element.newTabLink []
        { url = getText body |> Maybe.withDefault "no url"
        , label = el [ Font.color linkColor, Font.italic ] (text <| getArg 0 "link" args)
        }


renderStrong : FRender Mark2Msg
renderStrong generation blockOffset _ _ body sm =
    el [ Font.bold ] (renderElement generation blockOffset body)


renderItalic : FRender Mark2Msg
renderItalic generation blockOffset _ _ body sm =
    el [ Font.italic ] (renderElement generation blockOffset body)


highlight : FRender Mark2Msg
highlight generation blockOffset _ _ body sm =
    el [ Background.color yellowColor, Element.paddingXY 4 2 ] (renderElement generation blockOffset body)


highlightRGB : FRender Mark2Msg
highlightRGB generation blockOffset _ args body sm =
    let
        r =
            getInt 0 args

        g =
            getInt 1 args

        b =
            getInt 2 args
    in
    el [ Background.color (Element.rgb255 r g b), Element.paddingXY 4 2 ] (renderElement generation blockOffset body)


fontRGB : FRender Mark2Msg
fontRGB generation blockOffset _ args body sm =
    let
        r =
            getInt 0 args

        g =
            getInt 1 args

        b =
            getInt 2 args
    in
    el [ Font.color (Element.rgb255 r g b), Element.paddingXY 4 2 ] (renderElement generation blockOffset body)



-- MATH


renderTheorem : FRender Mark2Msg
renderTheorem generation blockOffset name args body sm =
    column [ spacing 3 ]
        [ row [ Font.bold ] [ text "Theorem." ]
        , el [] (renderElement generation blockOffset body)
        ]


renderMathDisplay : FRender Mark2Msg
renderMathDisplay generation blockOffset name args body sm =
    case getText body of
        Just content ->
            mathText generation blockOffset DisplayMathMode "foobar" content sm

        Nothing ->
            el [ Font.color redColor ] (text "Error rendering math !!!")


renderMath : FRender Mark2Msg
renderMath generation blockOffset name args body sm =
    case getText body of
        Just content ->
            mathText generation blockOffset InlineMathMode "foobar" content sm

        Nothing ->
            el [ Font.color redColor ] (text "Error rendering math !!!")


mathText : Int -> b -> DisplayMode -> String -> String -> Maybe Parser.SourceMap.SourceMap -> Element Mark2Msg
mathText generation blockOffset displayMode selectedId content sm =
    Html.Keyed.node "span" [] [ ( String.fromInt generation, mathText_ displayMode selectedId content sm ) ]
        |> Element.html


mathText_ : DisplayMode -> String -> String -> Maybe Parser.SourceMap.SourceMap -> Html Mark2Msg
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
    Element.rgb 0 0 0.8


redColor =
    Element.rgb 0.7 0 0


blueColor =
    Element.rgb 0 0 0.8


yellowColor =
    Element.rgb 1.0 1.0 0


violetColor =
    Element.rgb 0.4 0 0.8


codeColor =
    -- Element.rgb 0.2 0.5 1.0
    Element.rgb 0.4 0 0.8
