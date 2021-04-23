module CaYaTeX exposing (Data, init, render, update)

import Element as E
import Parser.Data
import Parser.Document
import Parser.Element
import Render.Elm


type alias Data =
    { content : String, generation : Int }


init : Int -> String -> Data
init generation text =
    { content = text, generation = generation }


update : Int -> String -> Data -> Data
update generation text _ =
    init generation text


render : String -> Data -> E.Element Parser.Element.Mark2Msg
render id data =
    Parser.Document.runloop data.generation (String.lines data.content)
        |> Parser.Document.toParsed
        |> List.map (Render.Elm.renderList (initState data.generation))
        |> E.column [ E.spacing 18 ]


initState k =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = Parser.Data.init Parser.Data.defaultConfig
    }
