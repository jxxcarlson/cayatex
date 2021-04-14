module Render.Elm exposing (Mark2Msg(..), renderElement, renderString)

import Dict exposing (Dict)
import Element exposing (Element, column, el, paragraph, row, spacing, text)
import Element.Font as Font
import Html
import Html.Attributes as HA
import Json.Encode
import Parser.Element
import Parser.SourceMap


type Mark2Msg
    = Mark2Msg


type DisplayMode
    = InlineMathMode
    | DisplayMathMode



{-

   type Parser.Element
       = Text String (Maybe SourceMap)
       | Parser.Element String (List String) Parser.Element (Maybe SourceMap)
       | LX (List Parser.Element) (Maybe SourceMap)

-}


redColor =
    Element.rgb 0.7 0 0


blueColor =
    Element.rgb 0 0 0.8


codeColor =
    Element.rgb 0.2 0.5 1.0


renderString : Int -> Int -> String -> Element Mark2Msg
renderString generation blockOffset str =
    case Parser.Element.parseList generation blockOffset str of
        Err _ ->
            row []
                [ el [ Font.color redColor ] (text "Parse error for ")
                , el [ Font.color blueColor ] (text str)
                ]

        Ok list ->
            row [] (List.map renderElement list)


renderElement : Parser.Element.Element -> Element Mark2Msg
renderElement element =
    let
        _ =
            Debug.log "ELEMENT" element
    in
    case element of
        Parser.Element.Text str _ ->
            el [] (text str)

        Parser.Element.Element name args body sm ->
            let
                _ =
                    Debug.log "BODY" body
            in
            renderWithDictionary name args body sm

        Parser.Element.LX list _ ->
            row [] (List.map renderElement list)


renderWithDictionary name args body sm =
    case Dict.get name renderElementDict of
        Nothing ->
            text (name ++ ": unimplemented")

        Just f ->
            case f of
                I g ->
                    g name args body sm

                B g ->
                    g name args body sm


type alias FRender a =
    String -> List String -> Parser.Element.Element -> Maybe Parser.SourceMap.SourceMap -> Element a


type RenderFunction a
    = I (FRender a)
    | B (FRender a)


type alias RenderElementDict a =
    Dict String (RenderFunction a)


renderElementDict : RenderElementDict Mark2Msg
renderElementDict =
    Dict.fromList
        [ ( "strong", I renderStrong )
        , ( "italic", I renderItalic )
        , ( "code", I renderCode )
        , ( "math", I renderMath )

        --, ( "mathDisplay", B renderMathDisplay )
        , ( "theorem", B renderTheorem )
        ]


renderMath : FRender Mark2Msg
renderMath _ args body sm =
    case getText body of
        Just content ->
            mathText InlineMathMode "foobar" content sm

        Nothing ->
            el [ Font.color redColor ] (text "Error rendering math !!!")



-- getText :  Parser.Element.Element -> Maybe String


getText : Parser.Element.Element -> Maybe String
getText element =
    let
        _ =
            Debug.log "EL" element
    in
    case element of
        Parser.Element.LX [ Parser.Element.Text content _ ] _ ->
            Just content

        _ ->
            Nothing


renderTheorem : FRender Mark2Msg
renderTheorem _ args body sm =
    column [ spacing 3 ]
        [ row [ Font.bold ] [ text "Theorem." ]
        , el [] (renderElement body)
        ]


renderCode : FRender Mark2Msg
renderCode _ _ body sm =
    el
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.size 14
        , Font.color codeColor
        ]
        (renderElement body)


renderStrong : FRender Mark2Msg
renderStrong _ _ body sm =
    el [ Font.bold ] (renderElement body)


renderItalic : FRender Mark2Msg
renderItalic _ _ body sm =
    el [ Font.italic ] (renderElement body)


mathText : DisplayMode -> String -> String -> Maybe Parser.SourceMap.SourceMap -> Element Mark2Msg
mathText displayMode selectedId content sm =
    let
        _ =
            Debug.log "mathText" content
    in
    Html.node "math-text"
        -- active sm selectedId  ++
        [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string content)

        -- , clicker sm
        -- , HA.id (makeId sm)
        ]
        []
        |> Element.html


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True



-- active : SourceMap -> String -> List (Attribute LaTeXMsg)
-- active sm selectedId =
--     let
--         id_ =
--             makeId sm
--     in
--     [ clicker sm, HA.id id_, highlight "#FAA" selectedId id_ ]
-- clicker sm =
--     onClick (SendSourceMap sm)
