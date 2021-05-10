module CaYaTeX exposing (CaYaTeXMsg, Data, init, render, renderString, renderToLaTeX, update)

import Element as E
import Parser.Data
import Parser.Document
import Parser.Element
import Render.Elm
import Render.LaTeX
import Render.Types as Types


type alias Data =
    { content : String, generation : Int }


type alias CaYaTeXMsg =
    Parser.Element.CYTMsg


init : Int -> String -> Data
init generation text =
    { content = text, generation = generation }


update : Int -> String -> Data -> Data
update generation text _ =
    init generation text


render : String -> Data -> List (E.Element Parser.Element.CYTMsg)
render id data =
    Parser.Document.runLoop data.generation (String.lines data.content)
        |> Parser.Document.toParsed
        |> List.map (Render.Elm.renderList (initState data.generation))

renderString : Int -> String -> E.Element Parser.Element.CYTMsg
renderString k str =
    -- CaYaTeX.render "id__" { content = str, generation = k } |> Element.map CYTMsg
    let
        state =
            Parser.Document.runLoop k (String.lines str)

        newState =
            initStateWithData k state.data
    in
    state
        |> Parser.Document.toParsed
        |> List.map (Render.Elm.renderList newState)
        |> E.column [ E.spacing 18 ]

initStateWithData k data =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = data
    }

renderToLaTeX : String -> String
renderToLaTeX =
    Render.LaTeX.render


initState : Int -> Types.RenderArgs
initState k =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = Parser.Data.init Parser.Data.defaultConfig
    }
