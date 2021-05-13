module CaYaTeX exposing
    ( CaYaTeXMsg
    , Data
    , getTitle
    , init
    , render
    , renderString
    , renderString2
    , renderToLaTeX
    , update
    )

import Element as E
import Parser.Data
import Parser.Document
import Parser.Element exposing (Element(..))
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


getTitle =
    Parser.Element.getTitle


renderString : Int -> String -> E.Element Parser.Element.CYTMsg
renderString k str =
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


astOfString : Int -> String -> List (List Element)
astOfString k str =
    let
        state =
            Parser.Document.runLoop k (String.lines str)

        newState =
            initStateWithData k state.data
    in
    state
        |> Parser.Document.toParsed


renderString2 : Int -> String -> { rendered : E.Element Parser.Element.CYTMsg, title : String }
renderString2 k str =
    let
        state =
            Parser.Document.runLoop k (String.lines str)

        ast =
            Parser.Document.toParsed state |> List.head |> Maybe.andThen List.head

        title =
            case ast of
                Nothing ->
                    "Err: title not found"

                Just (Element "title" _ stuff _) ->
                    case stuff of
                        LX [ Text str_ _ ] _ ->
                            str_

                        _ ->
                            "Err: title not found"

                _ ->
                    "Err: title not found"

        newState =
            initStateWithData k state.data

        rendered =
            state
                |> Parser.Document.toParsed
                |> List.map (Render.Elm.renderList newState)
                |> E.column [ E.spacing 18 ]
    in
    { rendered = rendered, title = title }


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
