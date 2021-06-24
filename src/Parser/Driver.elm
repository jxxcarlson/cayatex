module Parser.Driver exposing (parseLoop, pl)

import List.Extra
import Parser.Advanced as PA
import Parser.Data as Data exposing (Data)
import Parser.Element as Element exposing (Element(..))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Getters as Getters
import Parser.Loop as Loop
import Parser.Metadata as Metadata exposing (Metadata)
import Parser.RecoveryData as RecoveryData exposing (RecoveryData)
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

        Problem p e ->
            -- TODO: trouble?
            Problem p e


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


{-| TODO: Document how this works and how it is extended.
-}
handleError : List ParseError -> TextCursor Element -> TextCursor Element
handleError errors tc =
    let
        _ =
            Debug.log "ERRORS" errors

        mFirstError =
            List.head errors

        problem : Problem
        problem =
            mFirstError |> Maybe.map .problem |> Maybe.withDefault (UnHandledError 0) |> Debug.log "!! PROBLEM"

        _ =
            Debug.log "!!" "Dispatching ..."
    in
    handleError_ mFirstError tc




handleError_ : Maybe ParseError -> TextCursor Element -> TextCursor Element
handleError_ mFirstError tc =
    let
        textLines =
            String.lines tc.text |> Debug.log "HANDLE RightBracketError WITH"

        problem : Problem
        problem =
            mFirstError |> Maybe.map .problem |> Maybe.withDefault (UnHandledError 0) |> Debug.log "!! PROBLEM"

        newElement =
            Problem problem ( List.head textLines |> Maybe.withDefault "error text")

        errorColumn =
            mFirstError |> Maybe.map .col |> Maybe.withDefault 0

        errorRow =
            Maybe.map .row mFirstError |> Maybe.withDefault 0

        -- KEEP FOR NOW
        --errorLines : List String
        --errorLines =
        --    List.take errorRow textLines
    in
    { text = List.drop 1 textLines |> String.join "\n" |> (\s -> " \n " ++ s) |> Debug.log "TC.text"
    , block = ""
    , blockIndex = tc.blockIndex --
    , parsand = Nothing
    , parsed = newElement :: List.drop 1 tc.parsed -- throw away the erroneous parsand
    , stack = []
    , offset = tc.offset + 1 -- TODO: trouble!
    , count = tc.count
    , generation = tc.generation
    , data = tc.data
    , error = { status = NoError, correctedText = [] }
    }



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
