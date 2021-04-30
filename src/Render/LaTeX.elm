module Render.LaTeX exposing (..)

import Parser.Data
import Parser.Document
import Parser.Element exposing (Element)
import Render.Types as Types


render : String -> String
render sourceText =
    Parser.Document.runLoop 0 (String.lines sourceText)
        |> Parser.Document.toParsed
        |> renderList (initState 0)


renderList : Types.RenderArgs -> List (List Element) -> String
renderList renderArgs list_ =
    "not implemented"


initState : Int -> Types.RenderArgs
initState k =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = Parser.Data.init Parser.Data.defaultConfig
    }
