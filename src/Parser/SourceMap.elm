module Parser.SourceMap exposing (SourceMap)


type alias SourceMap =
    { blockOffset : Int
    , offset : Int
    , length : Int
    , generation : Int
    }
