module Parser.TextCursor exposing
    ( TextCursor, init, incrementBlockIndex, incrementBlockOffset
    , parseResult
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

-- import Parser.Element exposing (Expression)

import Parser.Data


{-| SourceText structure used by Parser.Loop.run as it progressively "eats" bites of
the text, accumulating the parsed bites in the list `parsed: List Expression`.
The `offset` represents the position of beginning of the current `text` in the
original text. It is used to properly construct Expressions, which contain
as a component a SourceMap, which locates the bite of text in the original input
text.
-}
type alias TextCursor e =
    { text : String
    , block : String
    , blockIndex : Int
    , parsed : List e
    , stack : List String
    , offset : Int
    , count : Int
    , generation : Int
    , data : Parser.Data.Data
    }


parseResult : TextCursor e -> List e
parseResult t =
    t.parsed


empty : TextCursor a
empty =
    { count = 0
    , text = ""
    , block = ""
    , blockIndex = 0
    , parsed = []
    , stack = []
    , offset = 0
    , generation = 0
    , data = Parser.Data.init Parser.Data.defaultConfig
    }


{-| Return a TextCursor with given chunkNumber and text
-}
init : Int -> Int -> String -> TextCursor a
init generation blockIndex text =
    { count = 0
    , text = text
    , block = ""
    , blockIndex = blockIndex
    , parsed = []
    , stack = []
    , offset = 0
    , generation = generation
    , data = Parser.Data.init Parser.Data.defaultConfig
    }


{-| -}
incrementBlockOffset : (Int -> a -> a) -> Int -> TextCursor a -> TextCursor a
incrementBlockOffset incrementer deltaBlockOffset tc =
    { tc | parsed = List.map (incrementer deltaBlockOffset) tc.parsed }


{-| -}
incrementBlockIndex : Int -> TextCursor a -> TextCursor a
incrementBlockIndex deltaBlockIndex tc =
    { tc | blockIndex = tc.blockIndex + deltaBlockIndex }
