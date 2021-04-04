module Parser.Expression exposing (..)

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Tool as Tool


type InlineExpression
    = Text String (Maybe SourceMap)
    | Inline String (List String) String (Maybe SourceMap)
    | List InlineExpression (Maybe SourceMap)


type alias SourceMap =
    { blockOffset : Int
    , length : Int
    , offset : Int
    , content : String
    , generation : Int
    }



-- PARSER


type alias Parser a =
    Parser.Parser Context Problem a


expression : Int -> Int -> Parser InlineExpression
expression generation lineNumber =
    Parser.oneOf [ inline generation lineNumber, text_ generation lineNumber [] ]



-- TEXT


text_ : Int -> Int -> List Char -> Parser InlineExpression
text_ generation lineNumber stopChars =
    Parser.map (\( t, s ) -> Text t s) (rawText generation lineNumber stopChars)


rawText : Int -> Int -> List Char -> Parser ( String, Maybe SourceMap )
rawText generation lineNumber stopChars =
    getChompedString generation lineNumber <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> not (List.member c stopChars))


rawText_ : List Char -> Parser { start : Int, length : Int, content : String }
rawText_ stopChars =
    Parser.succeed (\begin end content -> { start = begin, length = end - begin, content = String.slice begin end content })
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource


string : List Char -> Parser String
string stopChars =
    rawText_ stopChars |> Parser.map .content



-- INLINE ELEMENT


{-|

> run (inline 0 0) "[strong |0| stuff]"
> Ok (Inline "strong" ["0"] (" stuff") (Just { blockOffset = 0, content = "[strong |0| stuff]", generation = 0, length = 18, offset = 0 }))

> run (inline 0 0) "[strong stuff]"
> Ok (Inline "strong" [] "stuff" (Just { blockOffset = 0, content = "[strong stuff]", generation = 0, length = 14, offset = 0 }))

-}
inline : Int -> Int -> Parser InlineExpression
inline generation blockOffset =
    Parser.succeed (\start name ( args, body_ ) end source -> Inline name args body_ (Just { generation = generation, blockOffset = blockOffset, offset = start, length = end - start, content = source }))
        -- Parser.succeed (\start name end source -> Inline name [] "body" (Just {generation = generation, blockOffset = blockOffset, offset = start, length = end - start, content = source}))
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "[" (ExpectingToken "["))
        |= string [ ' ' ]
        |. Parser.symbol (Parser.Token " " (ExpectingToken "space"))
        |= argsAndBody
        |. Parser.symbol (Parser.Token "]" (ExpectingToken "]"))
        |= Parser.getOffset
        |= Parser.getSource


argsAndBody =
    Parser.oneOf [ argsAndBody_, body ]


argsAndBody_ =
    Parser.succeed (\args body_ -> ( args, body_ ))
        |. Parser.symbol (Parser.Token "[" (ExpectingToken "[ (args)"))
        |= Tool.manySeparatedBy (Parser.symbol (Parser.Token "," (ExpectingToken ","))) (string [ ' ', ']' ])
        |. Parser.symbol (Parser.Token "]" (ExpectingToken "] (args)"))
        |= string [ ']' ]


body =
    Parser.succeed (\body_ -> ( [], body_ ))
        |= string [ ']' ]


{-|

    Use this to parse a string and return information about its location in the source

-}
getChompedString : Int -> Int -> Parser a -> Parser ( String, Maybe SourceMap )
getChompedString generation lineNumber parser =
    let
        sm first_ last_ source_ =
            let
                src =
                    String.slice first_ last_ source_
            in
            ( src, Just { content = src, blockOffset = lineNumber, length = last_, offset = 0, generation = generation } )
    in
    Parser.succeed sm
        |= Parser.getOffset
        |. parser
        |= Parser.getOffset
        |= Parser.getSource
