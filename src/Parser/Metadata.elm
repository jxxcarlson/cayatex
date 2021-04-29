module Parser.Metadata exposing (Metadata, dummy, init)


type alias Metadata =
    { blockOffset : Int
    , offset : Int
    , length : Int
    , generation : Int
    , label : String
    }


dummy =
    { blockOffset = 0
    , offset = 0
    , length = 0
    , generation = 0
    , label = ""
    }


init generation lineNumber start finish =
    { blockOffset = lineNumber
    , offset = start
    , length = finish - start
    , generation = generation
    , label = ""
    }
