module Render.String exposing (renderElement, renderString)

import Dict exposing (Dict)
import Parser.Element as Element exposing (Element(..))


type DisplayMode
    = InlineMathMode
    | DisplayMathMode



{-

   type Element
       = Text String (Maybe SourceMap)
       | Element String (List String) Element (Maybe SourceMap)
       | LX (List Element) (Maybe SourceMap)

-}


renderString : String -> String
renderString str =
    case Element.parseList 0 0 str of
        Err _ ->
            "<span style='color:red'>Parser error for</span> <span style='color:blue'>" ++ str ++ "</span>"

        Ok list ->
            List.map renderElement list |> String.join "" |> div


renderString_ : String -> String
renderString_ str =
    case Element.parse 0 0 str of
        Err _ ->
            "Parser error for ((" ++ str ++ "))"

        Ok element ->
            renderElement element


renderElement : Element -> String
renderElement element =
    case element of
        Text str _ ->
            str

        Element name args body _ ->
            renderWithDictionary name args body

        LX list _ ->
            List.map renderElement list |> String.join " "

        Problem p e _->
            e


renderWithDictionary name args body =
    case Dict.get name renderElementDict of
        Nothing ->
            span (name ++ ": unimplemented")

        Just f ->
            case f of
                I g ->
                    g name args body

                B g ->
                    g name args body


type alias FRender =
    String -> List String -> Element -> String


type RenderFunction
    = I FRender
    | B FRender


type alias RenderElementDict =
    Dict String RenderFunction


renderElementDict : RenderElementDict
renderElementDict =
    Dict.fromList
        [ ( "strong", I renderStrong )
        , ( "italic", I renderItalic )
        , ( "code", I renderCode )
        , ( "math", I renderMath )
        , ( "mathDisplay", B renderMathDisplay )
        , ( "theorem", B renderTheorem )
        ]


renderTheorem _ args body =
    div ("<p><strong>Theorem.</strong></p><p>" ++ renderElement body ++ "</p>")


renderMath _ _ body =
    span ("\\(" ++ renderElement body ++ "\\)")


renderMathDisplay _ _ body =
    div ("\\[" ++ renderElement body ++ "\\]")


renderCode _ _ body =
    tag "code" (renderElement body)


renderStrong _ _ body =
    tag "strong" (renderElement body)


renderItalic _ _ body =
    tag "i" (renderElement body)



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



--- MATH
--
--
--mathText : DisplayMode -> String -> Html msg
--mathText sdisplayMode content =
--    Html.node "math-text"
--        (active sm selectedId
--            ++ [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
--               , HA.property "content" (Json.Encode.string content)
--               ]
--        )
--        []
--
--
--isDisplayMathMode : DisplayMode -> Bool
--isDisplayMathMode displayMode =
--    case displayMode of
--        InlineMathMode ->
--            False
--
--        DisplayMathMode ->
--            True
--
--
--inlineMathText :String -> String
--inlineMathText  str_ =
--    mathText InlineMathMode (String.trim str_)
--
--
--displayMathText : String -> String
--displayMathText str_ =
--    mathText DisplayMathMode (String.trim str_)
