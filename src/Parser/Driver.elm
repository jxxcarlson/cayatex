module Parser.Driver exposing (..)

import Parser.Loop as Loop
import Parser.Expression as Expression exposing(Expression(..))
import Parser.TextCursor exposing(TextCursor)
import Parser.Getters as Getters
import Parser.SourceMap exposing(SourceMap)

packet : Loop.Packet Expression
packet = {
       parser = Expression.parser
     , getSource = Getters.getSource
     , incrementOffset = incrementOffset
     , highlighter = Nothing
     , handleError = Nothing
  }


parseLoop : Int -> Int -> String -> TextCursor Expression
parseLoop generation initialLineNumber str = 
   Loop.parseLoop packet generation initialLineNumber str

pl str = parseLoop 0 0 str |> .parsed


{-| increment the offset field of the SourceMap component of an Expression
-}
incrementOffset : Int -> Expression -> Expression
incrementOffset delta expr =
    case expr of
        Text s sm ->
            Text s (incrementSourceMapOffset delta sm)

        Inline name args body_ sm ->
            Inline name args body_ (incrementSourceMapOffset delta sm)

        Block name args body_ sm ->
            Block name args body_ (incrementSourceMapOffset delta sm)

        List e sm ->
            List e (incrementSourceMapOffset delta sm)


incrementSourceMapOffset : Int -> Maybe SourceMap -> Maybe SourceMap
incrementSourceMapOffset delta sourceMap =
    case sourceMap of
        Just sm ->
            Just { sm | offset = sm.offset + delta }

        Nothing ->
            Nothing

