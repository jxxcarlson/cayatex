module Parser.SourceMap exposing (SourceMap, dummy)


type alias SourceMap =
    { blockOffset : Int
    , offset : Int
    , length : Int
    , generation : Int
    }


dummy =
    { blockOffset = 0
    , offset = 0
    , length = 0
    , generation = 0
    }
