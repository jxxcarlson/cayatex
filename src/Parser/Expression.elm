module Parser.Expression exposing (..)

{- (Expression(..), parser, strip, getSource, incrementOffset) -}

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.SourceMap exposing (SourceMap)
import Parser.Tool as Tool


type Expression
    = Text String (Maybe SourceMap)
    | Inline String (List String) String (Maybe SourceMap)
    | Block String (List Expression) (Maybe Expression) (Maybe SourceMap)
    | List Expression (Maybe SourceMap)



-- PARSER


type alias Parser a =
    Parser.Parser Context Problem a


parser : Int -> Int -> Parser Expression
parser generation lineNumber =
    Parser.oneOf [ inlineExpression standardInlineStopChars generation lineNumber, block generation lineNumber ]



-- BLOCK


block : Int -> Int -> Parser Expression
block generation lineNumber =
    Parser.succeed (\start (name,  args_, body_ ) end source -> Block name args_ body_ (Just { generation = generation, blockOffset = lineNumber, offset = start, length = end - start }))
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "|" (ExpectingToken "|"))
        |= Parser.oneOf [Parser.backtrackable (blockPath3 generation lineNumber), blockPath1 generation lineNumber, blockPath2 generation lineNumber]
        |= Parser.getOffset
        |= Parser.getSource





{-|
  "|theorem| Many primes!"
-}
blockPath1 generation lineNumber =
    Parser.succeed (\name body_ -> (name, [], body_))
        |= (string_ [ '|' ] |> Parser.map String.trim)
        |. symbol_ "|" "blockPath1"
        |= Tool.first (Tool.maybe (inlineExpression [ '|' ] generation lineNumber)) endOfBlock
        |. Parser.spaces

{-|
  "|theorem | Many primes!"
-}
blockPath2 generation lineNumber =
    Parser.succeed (\name body_ -> (name, [], body_))
        |= (string_ [ ' ' ] |> Parser.map String.trim)
        |. symbol_ " |" "blockPath2"
        |= Tool.first (Tool.maybe (inlineExpression [ '|' ] generation lineNumber)) endOfBlock
        |. Parser.spaces

{-|
  "|theorem title:Pythagoras| Many primes!"
-}
blockPath3 generation lineNumber =
    Parser.succeed (\name args_ body_ -> (name, args_, body_))
        |= (string_ [ ' ' ] |> Parser.map String.trim)
        |. symbol_ " " "blockPath3, 1"
        |= Tool.optionalList blockArgs
        |. symbol_ "|" "blockPath3, 2"
        |= Tool.first (Tool.maybe (inlineExpression [ '|' ] generation lineNumber)) endOfBlock
        |. Parser.spaces       

endOfBlock =
    Parser.symbol (Parser.Token "|end" (ExpectingToken "|end"))


blockArgs =
    Parser.succeed identity
        |= Tool.manySeparatedBy comma (inlineExpression [ '|', '[', ',' ] 0 0)
        |. Parser.spaces


symbol_ c e =
    Parser.symbol (Parser.Token c (ExpectingToken <| c ++ ": " ++ e))



-- INLINE


inlineExpression : List Char -> Int -> Int -> Parser Expression
inlineExpression stopChars generation lineNumber =
    -- TODO: think about the stop characters
    Parser.oneOf [ inline generation lineNumber, text_ generation lineNumber stopChars ]


standardInlineStopChars =
    [ '|', '[' ]


{-|

> run (inline 0 0) "[strong |0| stuff]"
> Ok (Inline "strong" ["0"] (" stuff") (Just { blockOffset = 0, content = "[strong |0| stuff]", generation = 0, length = 18, offset = 0 }))

> run (inline 0 0) "[strong stuff]"
> Ok (Inline "strong" [] "stuff" (Just { blockOffset = 0, content = "[strong stuff]", generation = 0, length = 14, offset = 0 }))

-}
inline : Int -> Int -> Parser Expression
inline generation blockOffset =
    Parser.succeed (\start name ( args, body_ ) end source -> Inline name args body_ (Just { generation = generation, blockOffset = blockOffset, offset = start, length = end - start }))
        -- Parser.succeed (\start name end source -> Inline name [] "body" (Just {generation = generation, blockOffset = blockOffset, offset = start, length = end - start, content = source}))
        |= Parser.getOffset
        |. leftBracket
        |= inlineName
        |= argsAndBody
        |. rightBracket
        |= Parser.getOffset
        |= Parser.getSource



leftBracket = symbol_ "[" ""
rightBracket = symbol_ "]" ""
oneSpace = symbol_ " " " "

inlineName = Tool.first (string_ [ ' ' ]) oneSpace

innerInlineArgs = Tool.manySeparatedBy comma (string [ ',', ']' ])

inlineArgs = Tool.between leftBracket innerInlineArgs rightBracket




argsAndBody =
    Parser.oneOf [ argsAndBody_, body ]


argsAndBody_ =
    Parser.succeed (\args body_ -> ( args, body_ ))
        |= inlineArgs
        |. Parser.spaces
        |= string_ [ ']' ]


comma_ =
    Parser.symbol (Parser.Token "," (ExpectingToken ","))


comma =
    Tool.first comma_ Parser.spaces


body =
    Parser.succeed (\body_ -> ( [], body_ ))
        |= string_ [ ']' ]



-- TEXT AND STRINGS


text_ : Int -> Int -> List Char -> Parser Expression
text_ generation lineNumber stopChars =
    Parser.map (\( t, s ) -> Text t s) (rawText generation lineNumber (\c -> c /= '|') stopChars)


rawText : Int -> Int -> (Char -> Bool) -> List Char -> Parser ( String, Maybe SourceMap )
rawText generation lineNumber prefixTest stopChars =
    getChompedString generation lineNumber <|
        Parser.succeed ()
            |. Parser.chompIf (\c -> prefixTest c) UnHandledError
            |. Parser.chompWhile (\c -> not (List.member c stopChars))


rawText_ : List Char -> Parser { start : Int, length : Int, content : String }
rawText_ stopChars =
    Parser.succeed (\begin end content -> { start = begin, length = end - begin, content = String.slice begin end content })
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource


string_ : List Char -> Parser String
string_ stopChars =
    rawText_ stopChars |> Parser.map .content


string stopChars =
    Tool.first (string_ stopChars) Parser.spaces



-- HELPERS


{-|

    Use this to parse a string and return information about its location in the source

-}
getChompedString : Int -> Int -> Parser a -> Parser ( String, Maybe SourceMap )
getChompedString generation lineNumber parser_ =
    let
        sm first_ last_ source_ =
            let
                src =
                    String.slice first_ last_ source_
            in
            ( src, Just { blockOffset = lineNumber, length = last_, offset = 0, generation = generation } )
    in
    Parser.succeed sm
        |= Parser.getOffset
        |. parser_
        |= Parser.getOffset
        |= Parser.getSource



-- UTILITY


{-| increment the offset field of the SourceMap component of an Expression
-}
incrementOffset : Int -> Expression -> Expression
incrementOffset delta expr =
    case expr of
        Text s sm ->
            Text s (incrementSourceMapOffset delta sm)

        Inline name args body_ sm ->
            Inline name args body_ (incrementSourceMapOffset delta sm)

        Block name args body_ sm ->
            Block name args body_ (incrementSourceMapOffset delta sm)

        List e sm ->
            List e (incrementSourceMapOffset delta sm)


incrementSourceMapOffset : Int -> Maybe SourceMap -> Maybe SourceMap
incrementSourceMapOffset delta sourceMap =
    case sourceMap of
        Just sm ->
            Just { sm | offset = sm.offset + delta }

        Nothing ->
            Nothing


{-| Set the SourceMap to Nothing
-}
strip : Expression -> Expression
strip expr =
    case expr of
        Text str _ ->
            Text str Nothing

        Inline name args body_ _ ->
            Inline name args body_ Nothing

        Block name args body_ _ ->
            Block name (List.map strip args) (Maybe.map strip body_) Nothing

        List expr_ _ ->
            List expr_ Nothing


getSource : Expression -> Maybe SourceMap
getSource expr =
    case expr of
        Text _ sm ->
            sm

        Inline _ _ _ sm ->
            sm

        Block _ _ _ sm ->
            sm

        List expr_ sm ->
            sm



-- getArgs : Expression -> Maybe Expression


getArgs_ expr =
    case expr of
        Text _ _ ->
            Nothing

        Inline _ args_ _ _ ->
            Nothing

        Block _ args_ _ _ ->
            Just args_

        List expr_ _ ->
            Nothing


getArgs expr =
    expr |> List.map getArgs_ |> List.map (Maybe.map (List.map strip))


getBody_ expr =
    case expr of
        Text str _ ->
            Nothing

        Inline _ _ body_ _ ->
            Nothing

        Block _ _ body_ _ ->
            body_

        List _ _ ->
            Nothing


getBody =
    List.map (getBody_ >> Maybe.map strip)
