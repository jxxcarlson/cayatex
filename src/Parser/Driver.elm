module Parser.Driver exposing (..)

import Parser.Loop as Loop
import Parser.Expression as Expression exposing(Expression(..))


type alias Packet a = {
        parser : (Int -> Int -> Parser a)
      , getSource : (a -> Maybe SourceMap)
      , incrementOffset : (Int -> a -> a)
      , highlighter : (a -> Maybe SourceMap -> a)
      , handleError : (TextCursor a -> List (Parser.DeadEnd Context Problem) -> TextCursor a)
      }

