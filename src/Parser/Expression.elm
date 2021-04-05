module Parser.Expression exposing (..)

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Tool as Tool
import Parser.SourceMap exposing (SourceMap)


-- type Expression = InlineExpression | List Expression


type Expression
    = Text String (Maybe SourceMap)
    | Inline String (List String) String (Maybe SourceMap)
    | Block String (List String) (Maybe Expression) (Maybe SourceMap)
    | List Expression



-- PARSER


type alias Parser a =
    Parser.Parser Context Problem a

--     Block -> "|" Name Args "|end" | "|" Name Args InlineExpression "|end"


block : Int -> Int -> Parser Expression
block generation lineNumber =
     Parser.succeed (\start name (args_, body_)  end source -> Block name args_ body_ (Just { generation = generation, blockOffset = lineNumber, offset = start, length = end - start, content = source }))
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "|" (ExpectingToken "|"))
        |= string_ [ ' ' ]
        |. Parser.symbol (Parser.Token " " (ExpectingToken "space"))
        |= blockArgsAndBody generation lineNumber
        --|. Parser.symbol (Parser.Token "|end" (ExpectingToken "|end"))
        |= Parser.getOffset
        |= Parser.getSource 



blockArgsAndBody : Int -> Int -> Parser (List String, Maybe Expression)
blockArgsAndBody generation lineNumber = 
   Parser.succeed (\args_ body_ -> (args_, body_))
     |= Tool.optionalList blockArgs
     |= (Tool.first (Tool.maybe (inlineExpression generation lineNumber)) endOfBlock)
     -- |. Parser.symbol (Parser.Token "|end" (ExpectingToken "|end"))
     |. Parser.spaces


endOfBlock = Parser.symbol (Parser.Token "|end" (ExpectingToken "|end"))
blockArgs = 
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "[" (ExpectingToken "[ (args)"))
        |= Tool.manySeparatedBy comma (string [ ',', ']' ])
        |. Parser.symbol (Parser.Token "]" (ExpectingToken "] (args)"))
        |. Parser.spaces

inlineExpression : Int -> Int -> Parser Expression
inlineExpression generation lineNumber =
    -- TODO: think about the stop characters
   Parser.oneOf [ inline generation lineNumber, text_ generation lineNumber ['|'] ]

-- UTILITY

{-| Set the SourceMap to Nothing
-}
strip : Expression -> Expression
strip expr = 
   case expr of 
     Text str _ -> Text str Nothing
     Inline name args body_ _ -> Inline name args body_ Nothing
     Block name args body_ _ -> Block name args body_ Nothing
     List expr_ -> List expr_


getSource : Expression -> Maybe SourceMap 
getSource expr =
  case expr of 
    Text _ sm -> sm
    Inline _ _ _ sm -> sm
    Block _ _ _ sm -> sm
    List expr_ -> Nothing

-- TEXT AND STRINGS


text_ : Int -> Int -> List Char -> Parser Expression
text_ generation lineNumber stopChars =
    Parser.map (\( t, s ) -> Text t s) (rawText generation lineNumber (\c -> c /= '|') stopChars)


rawText : Int -> Int -> (Char -> Bool) ->  List Char -> Parser ( String, Maybe SourceMap )
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

string stopChars = Tool.first (string_ stopChars) Parser.spaces


-- INLINE VARIANT


{-|

> run (inline 0 0) "[strong |0| stuff]"
> Ok (Inline "strong" ["0"] (" stuff") (Just { blockOffset = 0, content = "[strong |0| stuff]", generation = 0, length = 18, offset = 0 }))

> run (inline 0 0) "[strong stuff]"
> Ok (Inline "strong" [] "stuff" (Just { blockOffset = 0, content = "[strong stuff]", generation = 0, length = 14, offset = 0 }))

-}
inline : Int -> Int -> Parser Expression
inline generation blockOffset =
    Parser.succeed (\start name ( args, body_ ) end source -> Inline name args body_ (Just { generation = generation, blockOffset = blockOffset, offset = start, length = end - start, content = source }))
        -- Parser.succeed (\start name end source -> Inline name [] "body" (Just {generation = generation, blockOffset = blockOffset, offset = start, length = end - start, content = source}))
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "[" (ExpectingToken "["))
        |= string_ [ ' ' ]
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
        |= Tool.manySeparatedBy comma (string [ ',', ']' ])
        |. Parser.symbol (Parser.Token "]" (ExpectingToken "] (args)"))
        |. Parser.spaces
        |= string_ [ ']' ]


comma_ = Parser.symbol (Parser.Token "," (ExpectingToken ","))

comma = Tool.first comma_ Parser.spaces

body =
    Parser.succeed (\body_ -> ( [], body_ ))
        |= string_ [ ']' ]

-- HELPERS

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
