module Parser.Driver exposing (parseLoop, pl)

import List.Extra
import Parser.Advanced as PA
import Parser.Data as Data exposing (Data)
import Parser.Element as Element exposing (Element(..))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Getters as Getters
import Parser.Loop as Loop
import Parser.Metadata as Metadata exposing (Metadata)
import Parser.TextCursor as TextCursor exposing (ErrorStatus(..), TextCursor)


{-| The value of Loop.Packet that we need here
-}
packet : Loop.Packet Element
packet =
    { parser = Element.parser
    , getSource = Getters.getSource
    , incrementOffset = incrementOffset
    , highlighter = Nothing
    , handleError = Just handleError
    }


parseLoop : Int -> Int -> Data -> String -> TextCursor Element
parseLoop generation initialLineNumber data str =
    Loop.parseLoop packet generation initialLineNumber data str


pl : String -> List Element.SimpleElement
pl str =
    parseLoop 0 0 (Data.init Data.defaultConfig) str |> .parsed |> List.map Element.simplify


{-| increment the offset field of the SourceMap component of an Expression
-}
incrementOffset : Int -> Element -> Element
incrementOffset delta expr =
    case expr of
        Text s sm ->
            Text s (incrementSourceMapOffset delta sm)

        Element name args body_ sm ->
            Element name args body_ (incrementSourceMapOffset delta sm)

        LX e sm ->
            LX e (incrementSourceMapOffset delta sm)

        Problem p e sm ->
            -- TODO: trouble?
            Problem p e (incrementSourceMapOffset delta sm)


incrementSourceMapOffset : Int -> Maybe Metadata -> Maybe Metadata
incrementSourceMapOffset delta sourceMap =
    case sourceMap of
        Just sm ->
            Just { sm | offset = sm.offset + delta }

        Nothing ->
            Nothing



-- ERROR HANDLER


type alias ParseError =
    PA.DeadEnd Context Problem


{-| Transform the text cursor so that it does not have errors.
-}
handleError : List ParseError -> TextCursor Element -> TextCursor Element
handleError errors tc =
    let
        mFirstError =
            List.head errors

        problem : Problem
        problem =
            mFirstError |> Maybe.map .problem |> Maybe.withDefault (UnHandledError 0)

        textLines =
            String.lines tc.text

        smLength =
            Maybe.map String.length (List.head textLines) |> Maybe.withDefault 0

        meta =
            { blockOffset = 0
            , offset = 0
            , length = smLength
            , generation = tc.generation
            , label = "problem"
            }

        parsand =
            -- TODO: Nothing not right
            Problem problem (List.head textLines |> Maybe.withDefault "error text") (Just meta)

        errorColumn =
            mFirstError |> Maybe.map .col |> Maybe.withDefault 0

        errorRow =
            Maybe.map .row mFirstError |> Maybe.withDefault 0
    in
    { text = List.drop 1 textLines |> String.join "\n" |> (\s -> "\n" ++ s)
    , block = ""
    , blockIndex = tc.blockIndex --
    , parsand = Just parsand
    , parsed = parsand :: tc.parsed -- newElement :: List.drop 1 tc.parsed -- throw away the erroneous parsand
    , stack = []
    , offset = tc.offset + smLength -- TODO: trouble!?
    , count = tc.count
    , generation = tc.generation
    , data = Data.update parsand tc.data
    , error = { status = NoError, correctedText = [] }
    }



--Ok expr ->
--               let
--                   sourceMapLength =
--                       packet.getSource expr |> Maybe.map .length |> Maybe.withDefault 0
--
--                   parsand =
--                       newExpr packet tc expr
--
--                   data =
--                       Data.update parsand tc.data
--               in
--               Parser.Tool.Loop
--                   { tc
--                       | count = tc.count + 1
--                       , text = String.dropLeft sourceMapLength tc.text
--                       , block = tc.block ++ String.left sourceMapLength tc.text
--                       , parsand = Just parsand
--                       , parsed = Data.labelElement data parsand :: tc.parsed
--                       , offset = tc.offset + sourceMapLength
--                       , data = data
--                   }
-- HELPERS


newParsed tc_ lxError_ mRecoveryData =
    case mRecoveryData of
        Just rd ->
            rd.parseSubstitute :: tc_.parsed

        _ ->
            lxError_ :: tc_.parsed


makeNewText tc_ errorColumn_ mRecoveryData =
    case mRecoveryData of
        Just rd ->
            String.dropLeft rd.textTruncation tc_.text

        Nothing ->
            String.dropLeft errorColumn_ tc_.text


newStack tc_ errorText_ mRecoveryData =
    case mRecoveryData of
        Just _ ->
            if List.head tc_.stack == Just "highlight" then
                tc_.stack

            else
                "highlight" :: tc_.stack

        Nothing ->
            errorText_ :: "highlight" :: tc_.stack


parse__ : String -> List Element
parse__ str =
    case PA.run (Element.listParser 0 0) str of
        Ok list ->
            list

        Err _ ->
            -- TODO: vvv very bad code.  Fix this! vvv
            []



-- FAKE CHARACTERS


fakeLeftBracket =
    String.fromChar '⁅'


fakePipeSymbol =
    String.fromChar 'ǀ'


fakeRightBracket =
    String.fromChar '⁆'


listExample =
    """[list

[item Raspberry jam]

[item Sourdough bread]

]
"""
