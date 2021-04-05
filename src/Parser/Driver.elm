module Parser.Driver exposing (..)

import Parser.Loop as Loop
import Parser.Expression as Expression exposing(Expression(..))
import Parser.TextCursor exposing(TextCursor)

packet : Loop.Packet Expression
packet = {
       parser = Expression.parser
     , getSource = Expression.getSource
     , incrementOffset = Expression.incrementOffset
     , highlighter = Nothing
     , handleError = Nothing
  }


parseLoop : Int -> Int -> String -> TextCursor Expression
parseLoop generation initialLineNumber str = 
   Loop.parseLoop packet generation initialLineNumber str

pl str = parseLoop 0 0 str |> .parsed   