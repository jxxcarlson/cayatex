module Parser.Document exposing (renderString, processString)

import Element as E
import Parser.Data
import Parser.Element
import Parser.Lines as Lines
import Parser.Sections as Sections
import Parser.Types exposing (..)
import Render.Elm
import Render.Types


toStringList : Lines -> List String
toStringList lines =
    List.map (\r -> r.content) lines


type alias ProcessedDocument =
    { processedPrelude : State, processedSections : List State }


type alias RenderedState =
    { rendered : List (E.Element Parser.Element.CYTMsg), renderArgs : Render.Types.RenderArgs }


{-| Main and only exported function
-}
processString : Int -> String -> ProcessedDocument
processString generation str =
    processDocument generation (Sections.splitIntoSections str)


processDocument : Int -> Document -> ProcessedDocument
processDocument generation { prelude, sections } =
    let
        processedPrelude : State
        processedPrelude =
            Lines.process generation (toStringList prelude)

        dataOf : List State -> Parser.Data.Data
        dataOf acc_ =
            case List.head acc_ of
                Nothing ->
                    Parser.Data.init Parser.Data.defaultConfig

                Just state_ ->
                    state_.data

        folder : List String -> List State -> List State
        folder section acc =
            Lines.processWithData generation (dataOf acc) section :: acc

        processedSections_ : List State
        processedSections_ =
            List.foldl folder [ processedPrelude ] (List.map toStringList sections)

        data : Parser.Data.Data
        data =
            List.head processedSections_ |> Maybe.map .data |> Maybe.withDefault (Parser.Data.init Parser.Data.defaultConfig)

        processedSections =
            processedSections_
                |> List.map (\state -> { state | data = data })
                |> List.reverse
                |> List.drop 1
    in
    { processedPrelude = { processedPrelude | data = data }, processedSections = List.reverse processedSections }


renderString : Int -> String -> List (List (E.Element Parser.Element.CYTMsg))
renderString generation str =
    let
        { processedPrelude, processedSections } =
            processString generation str

        renderedPrelude : RenderedState
        renderedPrelude =
            renderState generation processedPrelude

        folder section acc =
            renderState generation section :: acc

        renderedSections : List RenderedState
        renderedSections =
            List.foldl (\section acc -> folder section acc) [] processedSections
    in
    renderedPrelude
        :: renderedSections
        |> List.map .rendered


renderState : Int -> State -> { rendered : List (E.Element Parser.Element.CYTMsg), renderArgs : Render.Types.RenderArgs }
renderState generation ast =
    let
        renderArgs =
            getRenderArgs generation ast
    in
    { rendered = List.map (Render.Elm.renderList renderArgs) (Lines.toParsed ast), renderArgs = renderArgs }


getRenderArgs k state =
    initRenderArgs k state.data


initRenderArgs k data =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = data
    }
