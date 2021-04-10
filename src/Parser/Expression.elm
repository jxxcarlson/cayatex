module Parser.Expression exposing (..)

{- (Expression(..), parser, strip, getSource, incrementOffset) -}

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.SourceMap exposing (SourceMap)
import Parser.Tool as T
import Parser.XString as XString


type Expression
    = Text String (Maybe SourceMap)
    | Inline String (List String) Expression (Maybe SourceMap)
    | Block String (List Expression) (Maybe Expression) (Maybe SourceMap)
    | LX (List Expression) (Maybe SourceMap)



-- PARSER


type alias Parser a =
    Parser.Parser Context Problem a


parser : Int -> Int -> Parser Expression
parser generation lineNumber =
    Parser.oneOf [ inlineExpression standardInlineStopChars generation lineNumber, block generation lineNumber ]



-- BLOCK


block : Int -> Int -> Parser Expression
block generation lineNumber =
    Parser.succeed (\start ( name, args_, body_ ) end source -> Block name args_ body_ (Just { generation = generation, blockOffset = lineNumber, offset = start, length = end - start }))
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "|" (ExpectingToken "|"))
        |= Parser.oneOf [ Parser.backtrackable (blockPath3 generation lineNumber), blockPath1 generation lineNumber, blockPath2 generation lineNumber ]
        |= Parser.getOffset
        |= Parser.getSource


{-| "|theorem| Many primes!"
-}
blockPath1 generation lineNumber =
    Parser.succeed (\name body_ -> ( name, [], body_ ))
        |= (string_ [ '|' ] |> Parser.map String.trim)
        |. symbol_ "|" "blockPath1"
        |= T.first (T.maybe (inlineExpression [ '|' ] generation lineNumber)) endOfBlock
        |. Parser.spaces


{-| "|theorem | Many primes!"
-}
blockPath2 generation lineNumber =
    Parser.succeed (\name body_ -> ( name, [], body_ ))
        |= (string_ [ ' ' ] |> Parser.map String.trim)
        |. symbol_ " |" "blockPath2"
        |= T.first (T.maybe (inlineExpression [ '|' ] generation lineNumber)) endOfBlock
        |. Parser.spaces


{-| "|theorem title:Pythagoras| Many primes!"
-}
blockPath3 generation lineNumber =
    Parser.succeed (\name args_ body_ -> ( name, args_, body_ ))
        |= (string_ [ ' ' ] |> Parser.map String.trim)
        |. symbol_ " " "blockPath3, 1"
        |= T.optionalList blockArgs
        |. symbol_ "|" "blockPath3, 2"
        |= T.first (T.maybe (inlineExpression [ '|' ] generation lineNumber)) endOfBlock
        |. Parser.spaces


endOfBlock =
    Parser.symbol (Parser.Token "|end" (ExpectingToken "|end"))


blockArgs =
    Parser.succeed identity
        |= T.manySeparatedBy comma (inlineExpression [ '|', '[', ',' ] 0 0)
        |. Parser.spaces



-- INLINE


inlineExpression : List Char -> Int -> Int -> Parser Expression
inlineExpression stopChars generation lineNumber =
    -- TODO: think about the stop characters
    Parser.oneOf [ inline generation lineNumber, text generation lineNumber ]


ie : Int -> Int -> Parser Expression
ie generation lineNumber =
    -- TODO: think about the stop characters
    Parser.oneOf [ text generation lineNumber, inline generation lineNumber ]


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
        |= Parser.getOffset
        |. leftBracket
        |= inlineName
        |= argsAndBody
        |. rightBracket
        |= Parser.getOffset
        |= Parser.getSource


inlineName =
    T.first (string_ [ ' ' ]) oneSpace


argsAndBody =
    Parser.oneOf [ argsAndBody_, bodyOnly ]


inlineArgs =
    T.between pipeSymbol innerInlineArgs pipeSymbol


innerInlineArgs =
    T.manySeparatedBy comma (string [ ',', '|' ])


body : Parser.Parser Context Problem Expression
body =
    -- (1) Parser.lazy (\_ -> inlineExpression [ ']' ] 0 0)
    -- (2) Parser.lazy (\_ -> Tool.many (inlineExpression [ '[', ']' ] 0 0) |> Parser.map (\list -> LX list Nothing))
    Parser.lazy (\_ -> inlineExpression [ ']' ] 0 0)


argsAndBody_ =
    Parser.succeed (\args body_ -> ( args, body_ ))
        |= inlineArgs
        |. Parser.spaces
        |= body


bodyOnly =
    Parser.succeed (\body_ -> ( [], body_ ))
        |= body


fubar =
    Parser.lazy (\_ -> T.many (Parser.lazy (\_ -> inlineExpression [ '[', ']' ] 0 0)))
        |> Parser.map (\le -> LX le Nothing)



-- TEXT AND STRINGS


text : Int -> Int -> Parser Expression
text generation lineNumber =
    Parser.inContext TextExpression <|
        (XString.text
            |> Parser.map (\data -> Text data.content (Just { blockOffset = lineNumber, offset = data.start, length = data.finish - data.start, generation = generation }))
        )


type alias StringData =
    { content : String, start : Int, finish : Int }


type alias SourceMap =
    { blockOffset : Int
    , offset : Int
    , length : Int
    , generation : Int
    }


textPS : (Char -> Bool) -> List Char -> Int -> Int -> Parser Expression
textPS prefixTest stopChars generation lineNumber =
    Parser.map (\( t, s ) -> Text t s) (rawText generation lineNumber prefixTest stopChars)


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
    T.first (string_ stopChars) Parser.spaces



-- SYMBOLS


comma_ =
    Parser.symbol (Parser.Token "," (ExpectingToken ","))


comma =
    T.first comma_ Parser.spaces


symbol_ c e =
    Parser.symbol (Parser.Token c (ExpectingToken <| c ++ ": " ++ e))


pipeSymbol =
    symbol_ "|" ""


leftBracket =
    symbol_ "[" ""


rightBracket =
    symbol_ "]" ""


oneSpace =
    symbol_ " " " "



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
