module Parser.Driver exposing (..)

import List.Extra
import Parser.Advanced as PA
import Parser.Element as Element exposing (Element(..))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Getters as Getters
import Parser.Loop as Loop
import Parser.RecoveryData as RecoveryData exposing (RecoveryData)
import Parser.SourceMap exposing (SourceMap)
import Parser.TextCursor exposing (TextCursor)


packet : Loop.Packet Element
packet =
    { parser = Element.element
    , getSource = Getters.getSource
    , incrementOffset = incrementOffset
    , highlighter = Nothing
    , handleError = Just handleError
    }


parseLoop : Int -> Int -> String -> TextCursor Element
parseLoop generation initialLineNumber str =
    Loop.parseLoop packet generation initialLineNumber str


pl str =
    parseLoop 0 0 str |> .parsed


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


incrementSourceMapOffset : Int -> Maybe SourceMap -> Maybe SourceMap
incrementSourceMapOffset delta sourceMap =
    case sourceMap of
        Just sm ->
            Just { sm | offset = sm.offset + delta }

        Nothing ->
            Nothing



-- ERROR HANDLER


{-| TODO: Document how this works and how it is extended.
-}
handleError : TextCursor Element -> List (PA.DeadEnd Context Problem) -> TextCursor Element
handleError tc_ e =
    let
        mFirstError =
            e
                |> List.head
                |> Debug.log "FIRST ERR"

        problem : Problem
        problem =
            mFirstError |> Maybe.map .problem |> Maybe.withDefault (UnHandledError 0)

        errorColumn =
            mFirstError |> Maybe.map .col |> Maybe.withDefault 0

        errorText =
            String.left errorColumn tc_.text

        mRecoveryData : Maybe RecoveryData
        mRecoveryData =
            RecoveryData.get tc_ problem

        lxError =
            Element "Error" [] (Text errorText Nothing) (Just { blockOffset = tc_.blockIndex, length = errorColumn, offset = tc_.offset + errorColumn, generation = tc_.generation })
    in
    case problem of
        ExpectingRightBracket ->
            handleRightBracketError tc_ mFirstError errorColumn mRecoveryData

        ExpectingPipe ->
            handlePipeError tc_ mFirstError errorColumn mRecoveryData

        _ ->
            unhandledError tc_ mFirstError errorColumn mRecoveryData lxError errorText


unhandledError tc_ mFirstError errorColumn mRecoveryData lxError errorText =
    { text = makeNewText tc_ errorColumn mRecoveryData
    , block = "?? TO DO"
    , blockIndex = tc_.blockIndex
    , parsed = newParsed tc_ lxError mRecoveryData
    , stack = newStack tc_ errorText mRecoveryData
    , offset = newOffset tc_ errorColumn mRecoveryData
    , count = tc_.count
    , generation = tc_.generation
    }



-- The handlers below will be rationalized and simplified.  Still in an experimental state.


handleRightBracketError : TextCursor Element -> Maybe (PA.DeadEnd Context Problem) -> Int -> Maybe RecoveryData -> TextCursor Element
handleRightBracketError tc_ mFirstError errorColumn mRecoveryData =
    let
        textLines =
            String.lines tc_.text
                |> Debug.log "handle TXT LINES"

        badText =
            (case List.head textLines of
                Nothing ->
                    "Oops, couldn't find your error text"

                Just str ->
                    str
            )
                |> Debug.log "BAD TEXT"

        correctedText =
            badText
                |> String.replace "[" fakeLeftBracket
                |> String.replace "|" fakePipeSymbol
                |> (\s -> s ++ " ?? " ++ fakeRightBracket)
                |> Debug.log "RBE, corrected"

        errorRow =
            Maybe.map .row mFirstError |> Maybe.withDefault 0 |> Debug.log "errorRow"

        errorLines : List String
        errorLines =
            List.take errorRow textLines
                |> Debug.log "handle ERR LINES"

        replacementText =
            "[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]"

        newTextLines =
            -- ("[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]") :: List.drop errorRow textLines
            List.Extra.setIf (\t -> t == badText) replacementText errorLines |> List.reverse |> Debug.log "NEW TEXT"

        newText =
            String.join "\n" (List.reverse newTextLines) |> Debug.log "NT"

        --sourceMapLength =
        --       packet.getSource expr |> Maybe.map .length |> Maybe.withDefault 0
        _ =
            Debug.log "x, he, parsed" tc_.parsed
    in
    { text = newText
    , block = "?? TO DO" --
    , blockIndex = tc_.blockIndex --
    , parsed = List.drop 1 tc_.parsed -- throw away the erroneous parsand
    , stack = [] -- not used
    , offset = newOffset tc_ errorColumn mRecoveryData |> Debug.log "RBEH, offset"
    , count = tc_.count
    , generation = tc_.generation
    }


handlePipeError : TextCursor Element -> Maybe (PA.DeadEnd Context Problem) -> Int -> Maybe RecoveryData -> TextCursor Element
handlePipeError tc_ mFirstError errorColumn mRecoveryData =
    let
        textLines : List String
        textLines =
            String.lines tc_.text
                |> Debug.log "(pipe) handle TXT LINES"

        badText : String
        badText =
            (case List.head textLines of
                Nothing ->
                    "Oops, couldn't find your error text"

                Just str ->
                    str
            )
                |> Debug.log "BAD TEXT"

        correctedText : String
        correctedText =
            badText
                |> String.replace "[" fakeLeftBracket
                |> String.replace "|" fakePipeSymbol
                |> (\s -> s ++ " ?? " ++ fakeRightBracket)

        errorRow : Int
        errorRow =
            Maybe.map .row mFirstError |> Maybe.withDefault 0

        errorLines : List String
        errorLines =
            List.take (errorRow - 1) textLines
                |> Debug.log "handle ERR LINES"

        replacementText : String
        replacementText =
            "[highlightRGB |255, 130, 130| missing trailing pipe symbol in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]"

        newTextLines : List String
        newTextLines =
            -- ("[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]") :: List.drop errorRow textLines
            List.Extra.setIf (\t -> t == badText) replacementText textLines |> List.reverse
    in
    { text = String.join "\n" (List.reverse newTextLines)
    , block = "?? TO DO"
    , blockIndex = tc_.blockIndex
    , parsed = parse__ (String.join "\n" errorLines)
    , stack = [] --newStack tc_ errorText mRecoveryData
    , offset = newOffset tc_ errorColumn mRecoveryData
    , count = tc_.count
    , generation = tc_.generation
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
    case PA.run (Element.elementList 0 0) str of
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
