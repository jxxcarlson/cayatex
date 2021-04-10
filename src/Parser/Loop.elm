module Parser.Loop exposing (..)

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context, Problem)
import Parser.Getters
import Parser.SourceMap exposing (SourceMap)
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Parser.Tool as ParserTool


type alias Parser a =
    Parser.Parser Context Problem a


type alias Packet a =
    { parser : Int -> Int -> Parser a
    , getSource : a -> Maybe SourceMap
    , incrementOffset : Int -> a -> a
    , highlighter : Maybe (a -> Maybe SourceMap -> a)
    , handleError : Maybe (TextCursor a -> List (Parser.DeadEnd Context Problem) -> TextCursor a)
    }


{-| parseLoop takes as input an integer representing a "chunkOffset" and
a string of source text, the "chunk." It returns a TextCursor, which
is a data structure which includes the parsed source text.
-}
parseLoop : Packet a -> Int -> Int -> String -> TextCursor a
parseLoop packet generation initialLineNumber str =
    ParserTool.loop (TextCursor.init generation initialLineNumber str) (nextCursor packet)


{-| nextCursor operates by running the expression parser on
`tc.text` with argument `tc.chunkNumber`. This argument is used
to track the location in the source text of the piece of text
parsed.

Recall that parseLoop is fed chunks of text by
Document.process. These chunks are "logical paragraphs,"
which one may think of as forming an array of strings indexed
by chunkNumber. A piece of text within a chunk is identified
by an offset and a length:

    piece =
        String.slice offset (offset + length) chunk

If parsing succeeds, resulting in a parsand `expr`, the textCursor
operated by parseLoop is updated:

    - the "program counter" tc.count is incremented
    - the piece of text corresponding to the parsand
      is removed from tc.text
    - `expr` is prepended to `tc.parsed`

-}
nextCursor : Packet a -> TextCursor a -> ParserTool.Step (TextCursor a) (TextCursor a)
nextCursor packet tc =
    if tc.text == "" || tc.count > 10 then
        -- TODO: that usage of count needs to be removed after bug is fixed
        ParserTool.Done { tc | parsed = List.reverse tc.parsed }

    else
        case Parser.run (packet.parser tc.generation tc.blockIndex) tc.text of
            Ok expr ->
                let
                    sourceMapLength =
                        packet.getSource expr |> Maybe.map .length |> Maybe.withDefault 0
                in
                ParserTool.Loop
                    { tc
                        | count = tc.count + 1
                        , text = String.dropLeft sourceMapLength tc.text
                        , block = tc.block ++ String.left sourceMapLength tc.text
                        , parsed = newExpr packet tc expr :: tc.parsed
                        , offset = tc.offset + sourceMapLength
                    }

            Err e ->
                case packet.handleError of
                    Nothing ->
                        ParserTool.Loop tc

                    Just he ->
                        ParserTool.Loop (he tc e)


newExpr packet tc_ expr =
    case List.head tc_.stack of
        Just "highlight" ->
            case packet.highlighter of
                Nothing ->
                    packet.incrementOffset tc_.offset expr

                Just hl ->
                    packet.incrementOffset tc_.offset (hl expr (packet.getSource expr))

        _ ->
            packet.incrementOffset tc_.offset expr



-- highlight : Expression -> SourceMap -> Expression
-- highlight expr_ sm =
--     Macro "blue" Nothing [ expr_ ] sm
-- {-| TODO: Document how this works and how it is extended.
-- -}
-- handleError : TextCursor -> List (Parser.DeadEnd Context Problem) -> TextCursor
-- handleError tc_ e =
--     let
--         mFirstError =
--             e
--                 |> List.head
--         problem =
--             mFirstError |> Maybe.map .problem |> Maybe.withDefault UnHandledError
--         errorColumn =
--             mFirstError |> Maybe.map .col |> Maybe.withDefault 0
--         errorText =
--             String.left errorColumn tc_.text
--         mRecoveryData : Maybe RecoveryData
--         mRecoveryData =
--             Parser.Problem.getRecoveryData tc_ problem
--         lxError =
--             LXError errorText problem { content = errorText, blockOffset = tc_.blockIndex, length = errorColumn, offset = tc_.offset + errorColumn, generation = tc_.generation }
--     in
--     if problem == ExpectingEndWord "\\end{theorem}" then
--         let
--             textLines =
--                 String.lines tc_.text
--             errorRow =
--                 Maybe.map .row mFirstError |> Maybe.withDefault 0
--             errorLines =
--                 List.take (errorRow - 1) textLines ++ [ "\\end{theorem}" ]
--             newTextLines =
--                 "\\red{^^ I fixed the theorem environment for you (unmatched begin-end pair); please correct it.}" :: "\\bigskip" :: List.drop errorRow textLines
--         in
--         { text = String.join "\n" newTextLines
--         , block = "?? TO DO"
--         , blockIndex = tc_.blockIndex
--         , parsed = parse (String.join "\n" errorLines)
--         , stack = [] --newStack tc_ errorText mRecoveryData
--         , offset = newOffset tc_ errorColumn mRecoveryData
--         , count = tc_.count
--         , generation = tc_.generation
--         }
--     else
--         { text = makeNewText tc_ errorColumn mRecoveryData
--         , block = "?? TO DO"
--         , blockIndex = tc_.blockIndex
--         , parsed = newParsed tc_ lxError mRecoveryData
--         , stack = newStack tc_ errorText mRecoveryData
--         , offset = newOffset tc_ errorColumn mRecoveryData
--         , count = tc_.count
--         , generation = tc_.generation
--         }
-- newOffset tc_ errorColumn_ mRecoveryData_ =
--     case mRecoveryData_ of
--         Just rd ->
--             tc_.offset + rd.deltaOffset
--         Nothing ->
--             tc_.offset + errorColumn_
-- newParsed tc_ lxError_ mRecoveryData =
--     case mRecoveryData of
--         Just rd ->
--             rd.parseSubstitute :: tc_.parsed
--         _ ->
--             lxError_ :: tc_.parsed
-- makeNewText tc_ errorColumn_ mRecoveryData =
--     case mRecoveryData of
--         Just rd ->
--             String.dropLeft rd.textTruncation tc_.text
--         Nothing ->
--             String.dropLeft errorColumn_ tc_.text
-- newStack tc_ errorText_ mRecoveryData =
--     case mRecoveryData of
--         Just _ ->
--             if List.head tc_.stack == Just "highlight" then
--                 tc_.stack
--             else
--                 "highlight" :: tc_.stack
--         Nothing ->
--             errorText_ :: "highlight" :: tc_.stack
