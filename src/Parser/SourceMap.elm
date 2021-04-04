module Parser.SourceMap exposing (SourceMap)

type alias SourceMap =
    { blockOffset : Int
    , length : Int
    , offset : Int
    , content : String
    , generation : Int
    }

