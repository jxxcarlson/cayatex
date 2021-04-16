module Parser.Element exposing (Element(..), element, elementList, parse, parseList)

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Tool as T
import Parser.XString as XString


type Element
    = Text String (Maybe SourceMap)
    | Element String (List String) Element (Maybe SourceMap)
    | LX (List Element) (Maybe SourceMap)



-- PARSER


parse : Int -> Int -> String -> Result (List (Parser.DeadEnd Context Problem)) Element
parse generation lineNumber str =
    Parser.run (element 1 2) str


parseList : Int -> Int -> String -> Result (List (Parser.DeadEnd Context Problem)) (List Element)
parseList generation lineNumber str =
    Parser.run (elementList generation lineNumber) str


type alias Parser a =
    Parser.Parser Context Problem a


elementList : Int -> Int -> Parser (List Element)
elementList generation lineNumber =
    T.many (element generation lineNumber)


element : Int -> Int -> Parser Element
element generation lineNumber =
    Parser.oneOf [ primitiveElement generation lineNumber, text generation lineNumber ]


{-|

> run (primitiveElement 0 0) "[strong |0| stuff]"
> Ok (Element "strong" ["0"] (" stuff") (Just { blockOffset = 0, content = "[strong |0| stuff]", generation = 0, length = 18, offset = 0 }))

> run (primitiveElement 0 0) "[strong stuff]"
> Ok (Element "strong" [] "stuff" (Just { blockOffset = 0, content = "[strong stuff]", generation = 0, length = 14, offset = 0 }))

-}
primitiveElement : Int -> Int -> Parser Element
primitiveElement generation blockOffset =
    Parser.inContext CInline <|
        Parser.succeed (\start name ( args, body_ ) end source -> Element name args body_ (Just { generation = generation, blockOffset = blockOffset, offset = start, length = end - start }))
            |= Parser.getOffset
            |. leftBracket
            |= elementName
            |= argsAndBody generation blockOffset
            |. rightBracket
            |= Parser.getOffset
            |= Parser.getSource


elementName =
    T.first (string_ [ ' ', '\n' ]) Parser.spaces


argsAndBody generation lineNumber =
    Parser.inContext (CInline_ "argsAndBody") <|
        Parser.oneOf [ argsAndBody_ generation lineNumber, bodyOnly generation lineNumber ]


elementArgs =
    Parser.inContext (CInline_ "inlineArgs") <|
        T.between pipeSymbol innerElementArgs pipeSymbol


innerElementArgs =
    T.manySeparatedBy comma (string [ ',', '|' ])


elementBody : Int -> Int -> Parser.Parser Context Problem Element
elementBody generation lineNumber =
    Parser.inContext (CInline_ "body") <|
        Parser.lazy (\_ -> T.many (element generation lineNumber) |> Parser.map (\list -> LX list Nothing))


argsAndBody_ generation lineNumber =
    Parser.succeed (\args body_ -> ( args, body_ ))
        |= elementArgs
        |. Parser.spaces
        |= elementBody generation lineNumber


bodyOnly generation lineNumber =
    Parser.succeed (\body_ -> ( [], body_ ))
        |= elementBody generation lineNumber



-- TEXT AND STRINGS


text : Int -> Int -> Parser Element
text generation lineNumber =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate XString.isNonLanguageChar
            |> Parser.map (\data -> Text data.content (Just { blockOffset = lineNumber, offset = data.start, length = data.finish - data.start, generation = generation }))
        )


textWithPredicate : (Char -> Bool) -> Int -> Int -> Parser Element
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


pipeSymbol =
    symbol_ "|" "Pipe"


leftBracket =
    symbol_ "[" "Left bracket"


rightBracket =
    symbol_ "]" "Right bracket"



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
