module Parser.Loop exposing (parseLoop, Packet)

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Data as Data exposing (Data)
import Parser.Element exposing (Element)
import Parser.Error exposing (Context, Problem)
import Parser.Getters
import Parser.Metadata exposing (Metadata)
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Parser.Tool as ParserTool


type alias Parser a =
    Parser.Parser Context Problem a


type alias Packet a =
    { parser : Int -> Int -> Parser a
    , getSource : a -> Maybe Metadata
    , incrementOffset : Int -> a -> a
    , highlighter : Maybe (a -> Maybe Metadata -> a)
    , handleError : Maybe (TextCursor a -> List (Parser.DeadEnd Context Problem) -> TextCursor a)
    }


{-| parseLoop takes as input an integer representing a "chunkOffset" and
a string of source text, the "chunk." It returns a TextCursor, which
is a data structure which includes the parsed source text.
-}
parseLoop : Packet Element -> Int -> Int -> Data -> String -> TextCursor Element
parseLoop packet generation initialLineNumber data str =
    ParserTool.loop (TextCursor.init generation initialLineNumber data str) (nextCursor packet)


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
nextCursor : Packet Element -> TextCursor Element -> ParserTool.Step (TextCursor Element) (TextCursor Element)
nextCursor packet tc =
    --let
    --    _ =
    --        Debug.log "(N, p, c)" ( tc.count, tc.parsand |> Maybe.map Parser.Getters.strip, tc.data.counters )
    --in
    if tc.text == "" || tc.count > 100 then
        -- TODO: that usage of count needs to be removed after bug is fixed
        ParserTool.Done { tc | parsed = List.reverse tc.parsed }

    else
        case Parser.run (packet.parser tc.generation tc.blockIndex) tc.text of
            Ok expr ->
                let
                    sourceMapLength =
                        packet.getSource expr |> Maybe.map .length |> Maybe.withDefault 0

                    parsand =
                        newExpr packet tc expr

                    data =
                        Data.update parsand tc.data
                in
                ParserTool.Loop
                    { tc
                        | count = tc.count + 1
                        , text = String.dropLeft sourceMapLength tc.text
                        , block = tc.block ++ String.left sourceMapLength tc.text
                        , parsand = Just parsand
                        , parsed = Data.labelElement data parsand :: tc.parsed
                        , offset = tc.offset + sourceMapLength
                        , data = data
                    }

            Err e ->
                case packet.handleError of
                    Nothing ->
                        ParserTool.Loop { tc | count = tc.count + 1 }

                    Just he ->
                        -- Continue loop with the text cursor that the error handler returns
                        --let
                        --    _ =
                        --        Debug.log "x, he tc e in LOOP" (he tc e)
                        --in
                        ParserTool.Loop (he tc e)


newExpr : Packet a -> TextCursor a -> a -> a
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
