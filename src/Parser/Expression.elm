module Parser.Expression exposing (Expression(..), parser)

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
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
    Parser.oneOf [ inlineExpression generation lineNumber, block generation lineNumber ]


manyExpression : Int -> Int -> Parser Expression
manyExpression generation lineNumber =
    Parser.inContext CManyExpression <|
        (T.many
            (parser generation lineNumber)
            |> Parser.map (\list -> LX list Nothing)
        )



-- BLOCK


block : Int -> Int -> Parser Expression
block generation lineNumber =
    Parser.succeed (\start ( name, args_, body_ ) end source -> Block name args_ body_ (Just { generation = generation, blockOffset = lineNumber, offset = start, length = end - start }))
        |= Parser.getOffset
        |. blockStartSymbol
        |= Parser.oneOf [ Parser.backtrackable (blockPath1 generation lineNumber), blockPath2 generation lineNumber ]
        |= Parser.getOffset
        |= Parser.getSource


{-| "|theorem|Many primes!|end"
-}
blockPath1 generation lineNumber =
    Parser.succeed (\name body_ -> ( name, [], body_ ))
        |= (string_ [ ' ', '|' ] |> Parser.map String.trim)
        |. blockSeparatorSymbol
        |= blockBody generation lineNumber
        |. Parser.spaces


{-| "|theorem title:Pythagoras| Many primes!|end"
-}
blockPath2 generation lineNumber =
    Parser.succeed (\name args_ body_ -> ( name, args_, body_ ))
        |= (string_ [ ' ' ] |> Parser.map String.trim)
        |. symbol_ " " "blockPath3, 1"
        |= T.optionalList blockArgs
        |. blockSeparatorSymbol
        |= blockBody generation lineNumber
        |. Parser.spaces


blockArgs =
    Parser.succeed identity
        |= T.manySeparatedBy comma (inlineExpressionWithPredicate XString.isNotExtendedLanguageChar 0 0)
        |. Parser.spaces


blockBody generation lineNumber =
    T.first (T.maybe (Parser.lazy (\_ -> manyExpression generation lineNumber))) endOfBlockSymbol



-- INLINE


inlineExpressionList : Int -> Int -> Parser Expression
inlineExpressionList generation lineNumber =
    Parser.inContext (CInline_ "inlineExpressionList") <|
        Parser.lazy (\_ -> T.many (inlineExpression generation lineNumber) |> Parser.map (\list -> LX list Nothing))


inlineExpressionWithPredicate : (Char -> Bool) -> Int -> Int -> Parser Expression
inlineExpressionWithPredicate predicate generation lineNumber =
    Parser.oneOf [ inline generation lineNumber, textWithPredicate predicate generation lineNumber ]


inlineExpression : Int -> Int -> Parser Expression
inlineExpression generation lineNumber =
    Parser.oneOf [ inline generation lineNumber, text2 generation lineNumber ]


{-|

> run (inline 0 0) "[strong |0| stuff]"
> Ok (Inline "strong" ["0"] (" stuff") (Just { blockOffset = 0, content = "[strong |0| stuff]", generation = 0, length = 18, offset = 0 }))

> run (inline 0 0) "[strong stuff]"
> Ok (Inline "strong" [] "stuff" (Just { blockOffset = 0, content = "[strong stuff]", generation = 0, length = 14, offset = 0 }))

-}
inline : Int -> Int -> Parser Expression
inline generation blockOffset =
    Parser.inContext CInline <|
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
    Parser.inContext (CInline_ "argsAndBody") <|
        Parser.oneOf [ argsAndBody_, bodyOnly ]


inlineArgs =
    Parser.inContext (CInline_ "inlineArgs") <|
        T.between pipeSymbol innerInlineArgs pipeSymbol


innerInlineArgs =
    T.manySeparatedBy comma (string [ ',', '|' ])


inlineBody : Parser.Parser Context Problem Expression
inlineBody =
    Parser.inContext (CInline_ "body") <|
        Parser.lazy (\_ -> T.many (inlineExpression 0 0) |> Parser.map (\list -> LX list Nothing))


argsAndBody_ =
    Parser.succeed (\args body_ -> ( args, body_ ))
        |= inlineArgs
        |. Parser.spaces
        |= inlineBody


bodyOnly =
    Parser.succeed (\body_ -> ( [], body_ ))
        |= inlineBody



-- TEXT AND STRINGS


text : Int -> Int -> Parser Expression
text generation lineNumber =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate XString.isNonLanguageChar
            |> Parser.map (\data -> Text data.content (Just { blockOffset = lineNumber, offset = data.start, length = data.finish - data.start, generation = generation }))
        )


text2 : Int -> Int -> Parser Expression
text2 generation lineNumber =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate XString.isNotExtendedLanguageChar
            |> Parser.map (\data -> Text data.content (Just { blockOffset = lineNumber, offset = data.start, length = data.finish - data.start, generation = generation }))
        )


textWithPredicate : (Char -> Bool) -> Int -> Int -> Parser Expression
textWithPredicate predicate generation lineNumber =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate predicate
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


string stopChars =
    T.first (string_ stopChars) Parser.spaces


string_ : List Char -> Parser String
string_ stopChars =
    rawText_ stopChars |> Parser.map .content


rawText_ : List Char -> Parser { start : Int, length : Int, content : String }
rawText_ stopChars =
    Parser.succeed (\begin end content -> { start = begin, length = end - begin, content = String.slice begin end content })
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource



-- SYMBOLS


symbol_ c e =
    Parser.symbol (Parser.Token c (ExpectingToken e))


comma_ =
    symbol_ "," "Comma"


comma =
    T.first comma_ Parser.spaces


blockStartSymbol =
    symbol_ "{" "Block start"


blockSeparatorSymbol =
    symbol_ "|" "Block separator"


endOfBlockSymbol =
    Parser.symbol (Parser.Token "}" (ExpectingToken "}"))


pipeSymbol =
    symbol_ "|" "Pipe"


leftBracket =
    symbol_ "[" "Left bracket"


rightBracket =
    symbol_ "]" "Right bracket"


oneSpace =
    symbol_ " " "One space"



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
