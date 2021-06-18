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
import Parser.TextCursor as TextCursor exposing (TextCursor)


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


pl : String -> List Element
pl str =
    parseLoop 0 0 (Data.init Data.defaultConfig) str |> .parsed


pl2 str =
    pl str |> List.map Getters.strip


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


incrementSourceMapOffset : Int -> Maybe Metadata -> Maybe Metadata
incrementSourceMapOffset delta sourceMap =
    case sourceMap of
        Just sm ->
            Just { sm | offset = sm.offset + delta }

        Nothing ->
            Nothing



-- ERROR HANDLER


type alias ParseError  =
    PA.DeadEnd Context Problem


{-| TODO: Document how this works and how it is extended.
-}
handleError : TextCursor Element -> List ParseError -> TextCursor Element
handleError tc_ errors =
    let
        mFirstError =
             List.head errors

        problem : Problem
        problem =
            mFirstError |> Maybe.map .problem |> Maybe.withDefault (UnHandledError 0)

        errorColumn =
            mFirstError |> Maybe.map .col |> Maybe.withDefault 0

        errorText =
            -- Example: if the input text is "foo [b bar", then
            -- the error text is "[b bar"
            -- But if the input text is "foo [b bar\n\nabc", then
            -- the error text is "b b" which is INCORRECT.
            String.left errorColumn tc_.text --|> Debug.log "ERROR TEXT"

        mRecoveryData : Maybe RecoveryData
        mRecoveryData =
            RecoveryData.get tc_ problem -- |> Debug.log "RECOVERY DATA"

        lxError =
            Element "Error" [] (Text errorText Nothing) (Just { blockOffset = tc_.blockIndex, length = errorColumn, offset = tc_.offset + errorColumn, generation = tc_.generation, label = "" })
    in
    case problem of
        ExpectingRightBracket ->
            handleRightBracketError tc_ mFirstError errorColumn mRecoveryData

        ExpectingLeftBracket ->
            handleLeftBracketError tc_ mFirstError errorColumn mRecoveryData

        ExpectingPipe ->
            handlePipeError tc_ mFirstError errorColumn mRecoveryData

        _ ->
            unhandledError tc_ mFirstError errorColumn mRecoveryData lxError errorText


unhandledError tc_ mFirstError errorColumn mRecoveryData lxError errorText =
    { text = makeNewText tc_ errorColumn mRecoveryData
    , block = "?? TO DO"
    , blockIndex = tc_.blockIndex
    , parsand = Nothing
    , parsed = newParsed tc_ lxError mRecoveryData
    , stack = newStack tc_ errorText mRecoveryData
    , offset = newOffset tc_ errorColumn mRecoveryData
    , count = tc_.count
    , generation = tc_.generation
    , data = tc_.data
    , error = { status = TextCursor.UnhandledError, correctedText = [] }
    }



-- The handlers below will be rationalized and simplified.  Still in an experimental state.


handleRightBracketError : TextCursor Element -> Maybe ParseError -> Int -> Maybe RecoveryData -> TextCursor Element
handleRightBracketError tc_ mFirstError errorColumn mRecoveryData =
    let

        textLines =
            String.lines tc_.text

        badText =
            case List.head textLines of
                Nothing ->
                    "Oops, couldn't find your error text"

                Just str ->
                    str

        correctedText =
            badText
                |> String.replace "[" fakeLeftBracket
                |> String.replace "|" fakePipeSymbol
                |> (\s -> s ++ fakeRightBracket)

        errorRow =
            Maybe.map .row mFirstError |> Maybe.withDefault 0

        errorLines : List String
        errorLines =
            List.take errorRow textLines

        replacementText =
            "[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]"

        newTextLines =
            -- ("[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]") :: List.drop errorRow textLines
            List.Extra.setIf (\t -> t == badText) replacementText errorLines
                |> List.reverse

        newText =
            String.join "\n" (List.reverse newTextLines)
    in
    { text = newText
    , block = "?? TO DO" --
    , blockIndex = tc_.blockIndex --
    , parsand = Nothing
    , parsed = List.drop 1 tc_.parsed -- throw away the erroneous parsand
    , stack = [] -- not used
    , offset = newOffset tc_ errorColumn mRecoveryData
    , count = tc_.count
    , generation = tc_.generation
    , data = tc_.data
    , error = { status = TextCursor.RightBracketError, correctedText = newTextLines }
    }


handleLeftBracketError : TextCursor Element -> Maybe (PA.DeadEnd Context Problem) -> Int -> Maybe RecoveryData -> TextCursor Element
handleLeftBracketError tc_ mFirstError errorColumn mRecoveryData =
    let
        textLines =
            String.lines tc_.text

        badText =
            case List.head textLines of
                Nothing ->
                    "Oops, couldn't find your error text"

                Just str ->
                    str

        name =
            String.replace "[" "" tc_.block |> String.words |> List.head |> Maybe.withDefault "NAME"

        errorRow =
            Maybe.map .row mFirstError |> Maybe.withDefault 0

        errorLines : List String
        errorLines =
            List.take errorRow textLines

        replacementText =
            "[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| " ++ fakeLeftBracket ++ " " ++ name ++ " " ++ tc_.block ++ " " ++ fakeRightBracket ++ "]"

        newTextLines =
            -- ("[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]") :: List.drop errorRow textLines
            List.Extra.setIf (\t -> t == badText) replacementText errorLines
                |> List.reverse

        newText =
            String.join "\n" (List.reverse newTextLines)
    in
    { text = newText
    , block = "?? TO DO" --
    , blockIndex = tc_.blockIndex --
    , parsand = Nothing
    , parsed = List.drop 1 tc_.parsed -- throw away the erroneous parsand
    , stack = [] -- not used
    , offset = newOffset tc_ errorColumn mRecoveryData
    , count = tc_.count
    , generation = tc_.generation
    , data = tc_.data
    , error = { status = TextCursor.LeftBracketError, correctedText = newTextLines }
    }


handlePipeError : TextCursor Element -> Maybe (PA.DeadEnd Context Problem) -> Int -> Maybe RecoveryData -> TextCursor Element
handlePipeError tc_ mFirstError errorColumn mRecoveryData =
    let
        textLines : List String
        textLines =
            String.lines tc_.text

        badText : String
        badText =
            case List.head textLines of
                Nothing ->
                    "Oops, couldn't find your error text"

                Just str ->
                    str

        correctedText : String
        correctedText =
            badText
                |> String.replace "[" fakeLeftBracket
                |> String.replace "|" fakePipeSymbol
                |> (\s -> s ++ fakeRightBracket)

        errorRow : Int
        errorRow =
            Maybe.map .row mFirstError |> Maybe.withDefault 0

        errorLines : List String
        errorLines =
            List.take (errorRow - 1) textLines

        replacementText : String
        replacementText =
            "[highlightRGB |255, 130, 130| missing trailing pipe symbol in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]"

        newTextLines : List String
        newTextLines =
            List.Extra.setIf (\t -> t == badText) replacementText textLines |> List.reverse
    in
    { text = String.join "\n" (List.reverse newTextLines)
    , block = "?? TO DO"
    , blockIndex = tc_.blockIndex
    , parsed = parse__ (String.join "\n" errorLines)
    , parsand = Nothing
    , stack = [] --newStack tc_ errorText mRecoveryData
    , offset = newOffset tc_ errorColumn mRecoveryData
    , count = tc_.count
    , generation = tc_.generation
    , data = tc_.data
    , error = { status = TextCursor.PipeError, correctedText = newTextLines }
    }



-- HELPERS


newOffset tc_ errorColumn_ mRecoveryData_ =
    case mRecoveryData_ of
        Just rd ->
            tc_.offset + rd.deltaOffset

        Nothing ->
            tc_.offset + errorColumn_


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
