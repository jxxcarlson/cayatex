module Render.String exposing (renderElement, renderString)

import Dict exposing (Dict)
import Parser.Element as Element exposing (Element(..))



{-

   type Element
       = Text String (Maybe SourceMap)
       | Element String (List String) Element (Maybe SourceMap)
       | LX (List Element) (Maybe SourceMap)

-}


renderString : String -> String
renderString str =
    case Element.parse str of
        Err _ ->
            "Parser error for ((" ++ str ++ "))"

        Ok element ->
            renderElement element


renderElement : Element -> String
renderElement element =
    case element of
        Text str _ ->
            span str

        Element name args body _ ->
            renderWithDictionary name args body

        LX list _ ->
            List.map renderElement list |> String.join " "


renderWithDictionary name args body =
    case Dict.get name renderElementDict of
        Nothing ->
            span (name ++ ": unimplemented")

        Just f ->
            f name args body


renderStrong name _ body =
    tag "strong" (renderElement body)


type alias FRender =
    String -> List String -> Element -> String


type alias RenderElementDict =
    Dict String FRender


renderElementDict : RenderElementDict
renderElementDict =
    Dict.fromList
        [ ( "strong", renderStrong )
        ]



-- HELPERS


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
