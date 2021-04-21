module CayatexBenchmark exposing (suite)

import Benchmark exposing (..)
import Data
import Parser.Data
import Parser.Document
import Render.Elm
import SourceText



-- render : Int -> String -> Element Msg


parseAndRender k str =
    Parser.Document.runloop k (String.lines str)
        |> Parser.Document.toParsed
        |> List.map (Render.Elm.renderList (initState k))


initState k =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = Parser.Data.init Parser.Data.defaultConfig
    }


suite : Benchmark
suite =
    describe "CaYaTex"
        [ -- nest as many descriptions as you like
          describe "parseAndRender"
            [ benchmark "doc3" <|
                \_ -> parseAndRender 0 SourceText.doc3

            --, benchmark "doc2" <|
            --    \_ -> parseAndRender 0 SourceText.doc2
            ]
        ]
