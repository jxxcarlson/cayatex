module Render.Abstract exposing (..)

import Dict exposing (Dict)
import Parser.Element exposing (Element(..))
import Parser.Metadata exposing (Metadata)


type Renderer
    = Renderer Name Args Body


type alias Name =
    String


type alias Args =
    List String


type alias Body =
    Maybe Element


type alias RenderArgs =
    { width : Int
    , selectedId : String
    , generation : Int
    , blockOffset : Int
    }


type alias FRender a =
    RenderArgs -> String -> List String -> Element -> Maybe Metadata -> a


type alias RenderElementDict a =
    Dict String (FRender a)


renderElement : RenderArgs -> Element -> a
renderElement renderArgs element =
    case element of
        Text str _ ->
            renderText str

        Element name args body sm ->
            renderWithDictionary renderArgs name args body sm

        LX list_ _ ->
            renderList (List.map (renderElement renderArgs) list_)


renderText : String -> a
renderText str =
    Debug.todo str


renderList : List a -> a
renderList list =
    Debug.todo list


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


renderElementDict : RenderElementDict a
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
