module Render.Elm exposing (..)

import Dict exposing (Dict)
import Element exposing (Element, column, el, paragraph, row, text)
import Element.Font as Font
import Parser.Element
import Parser.SourceMap


type Mark2Msg
    = Mark2Msg



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
    case element of
        Parser.Element.Text str _ ->
            el [] (text str)

        Parser.Element.Element name args body sm ->
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

        --, ( "code", I renderCode )
        --, ( "math", I renderMath )
        --, ( "mathDisplay", B renderMathDisplay )
        --, ( "theorem", B renderTheorem )
        ]



--renderTheorem _ args body =
--    div ("<p><strong>Theorem.</strong></p><p>" ++ renderElement body ++ "</p>")
--
--
--renderMath _ _ body =
--    span ("\\(" ++ renderElement body ++ "\\)")
--
--
--renderMathDisplay _ _ body =
--    div ("\\[" ++ renderElement body ++ "\\]")
--
--renderCode _ _ body =
--    tag "code" (renderElement body)
--
-- renderStrong : a -> b -> Parser.Element.Element -> Maybe Parser.SourceMap.SourceMap -> Element Mark2Msg


renderStrong : FRender Mark2Msg
renderStrong _ _ body sm =
    el [ Font.bold ] (renderElement body)


renderItalic : FRender Mark2Msg
renderItalic _ _ body sm =
    el [ Font.italic ] (renderElement body)



-- HELPERS


div : String -> String
div str =
    tag "div" str


span : String -> String
span str =
    tag "span" str


beginTag : String -> String
beginTag str =
    "<" ++ str ++ ">"


endTag : String -> String
endTag str =
    "</" ++ str ++ ">"


tag : String -> String -> String
tag tag_ str =
    beginTag tag_ ++ str ++ endTag tag_
