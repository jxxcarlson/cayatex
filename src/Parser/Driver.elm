module Parser.Driver exposing (..)

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
    , handleError = Nothing
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

        problem : Problem
        problem =
            mFirstError |> Maybe.map .problem |> Maybe.withDefault UnHandledError

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
    if problem == ExpectingRightBracket then
        let
            textLines =
                String.lines tc_.text

            errorRow =
                Maybe.map .row mFirstError |> Maybe.withDefault 0

            errorLines =
                List.take (errorRow - 1) textLines ++ [ "\\]" ]

            newTextLines =
                "\\red{^^ I fixed the offending element for you (missing right bracket); please correct it.}" :: List.drop errorRow textLines
        in
        { text = String.join "\n" newTextLines
        , block = "?? TO DO"
        , blockIndex = tc_.blockIndex
        , parsed = parse__ (String.join "\n" errorLines)
        , stack = [] --newStack tc_ errorText mRecoveryData
        , offset = newOffset tc_ errorColumn mRecoveryData
        , count = tc_.count
        , generation = tc_.generation
        }

    else
        { text = makeNewText tc_ errorColumn mRecoveryData
        , block = "?? TO DO"
        , blockIndex = tc_.blockIndex
        , parsed = newParsed tc_ lxError mRecoveryData
        , stack = newStack tc_ errorText mRecoveryData
        , offset = newOffset tc_ errorColumn mRecoveryData
        , count = tc_.count
        , generation = tc_.generation
        }


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
