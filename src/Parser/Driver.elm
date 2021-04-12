module Parser.Driver exposing (..)

import Parser.Element as Element exposing (Element(..))
import Parser.Getters as Getters
import Parser.Loop as Loop
import Parser.SourceMap exposing (SourceMap)
import Parser.TextCursor exposing (TextCursor)


packet : Loop.Packet Element
packet =
    { parser = Element.element
    , getSource = Getters.getSource
    , incrementOffset = incrementOffset
    , highlighter = Nothing
    , handleError = Nothing
    }


parseLoop : Int -> Int -> String -> TextCursor Element
parseLoop generation initialLineNumber str =
    Loop.parseLoop packet generation initialLineNumber str


pl str =
    parseLoop 0 0 str |> .parsed


{-| increment the offset field of the SourceMap component of an Expression
-}
incrementOffset : Int -> Element -> Element
incrementOffset delta expr =
    case expr of
        Text s sm ->
            Text s (incrementSourceMapOffset delta sm)

        Element name args body_ sm ->
            Element name args body_ (incrementSourceMapOffset delta sm)

        LX e sm ->
            LX e (incrementSourceMapOffset delta sm)


incrementSourceMapOffset : Int -> Maybe SourceMap -> Maybe SourceMap
incrementSourceMapOffset delta sourceMap =
    case sourceMap of
        Just sm ->
            Just { sm | offset = sm.offset + delta }

        Nothing ->
            Nothing
