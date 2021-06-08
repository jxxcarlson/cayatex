module Parser.Element exposing
    ( CYTMsg(..)
    , Element(..)
    , listParser
    , parse
    , parseList
    , parser
    , r0
    , r1
    , r1b
    , r2
    , r3
    , rawString
    )

{-


-}

import Maybe.Extra
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Metadata as Metadata exposing (Metadata)
import Parser.RawString as RawString
import Parser.Tool as T
import Parser.XString as XString


type Element
    = Text String (Maybe Metadata)
    | Element String (List String) Element (Maybe Metadata)
    | LX (List Element) (Maybe Metadata)


type alias Parser a =
    Parser.Parser Context Problem a


type alias ParseError =
    Parser.DeadEnd Context Problem


type CYTMsg
    = Mark2Msg
    | CYDocumentLink String



-- PARSER


r0 =
    "[c raw#abc#]"


r1 =
    "[c raw#[abc]#]"


r1b =
    "[c raw#[abc#]"


r2 =
    "[c raw#[3]#]"


r3 =
    "[c raw#[3#]"


parse : Int -> Int -> String -> Result (List ParseError) Element
parse generation lineNumber str =
    Parser.run (parser generation lineNumber) str


parseList : Int -> Int -> String -> Result (List ParseError) (List Element)
parseList generation lineNumber str =
    Parser.run (listParser generation lineNumber) str


listParser : Int -> Int -> Parser (List Element)
listParser generation lineNumber =
    T.many (parser generation lineNumber)


parser : Int -> Int -> Parser Element
parser generation lineNumber =
    Parser.oneOf [ primitiveElement generation lineNumber, text generation lineNumber ]


{-|

> run (primitiveElement 0 0) "[strong |0| stuff]"
> Ok (Element "strong" ["0"] (" stuff") (Just { blockOffset = 0, content = "[strong |0| stuff]", generation = 0, length = 18, offset = 0 }))

> run (primitiveElement 0 0) "[strong stuff]"
> Ok (Element "strong" [] "stuff" (Just { blockOffset = 0, content = "[strong stuff]", generation = 0, length = 14, offset = 0 }))

-}
primitiveElement : Int -> Int -> Parser Element
primitiveElement generation blockOffset =
    Parser.inContext CElement <|
        -- Parser.succeed (\start name ( args, body_ ) end source -> Element name args body_ (Just { generation = generation, blockOffset = blockOffset, offset = start, length = end - start }))
        -- TODO: is this correct?
        Parser.succeed (\start name ( args, body_ ) end source -> Element name args body_ (Just (Metadata.init generation blockOffset start end)))
            |= Parser.getOffset
            |. leftBracket
            |= elementName
            |= argsAndBody generation blockOffset
            |. Parser.spaces
            |. rightBracket
            |= Parser.getOffset
            |= Parser.getSource


elementName =
    T.first (string_ [ '[', ']', ' ', '\n' ]) Parser.spaces


argsAndBody generation lineNumber =
    Parser.inContext CArgsAndBody <|
        Parser.oneOf [ argsAndBody_ generation lineNumber, bodyOnly generation lineNumber ]


elementArgs =
    Parser.inContext CArgs <|
        T.between pipeSymbol innerElementArgs pipeSymbol


innerElementArgs =
    T.manySeparatedBy comma (string [ ',', '|' ])


elementBody : Int -> Int -> Parser.Parser Context Problem Element
elementBody generation lineNumber =
    Parser.inContext CBody <|
        Parser.lazy (\_ -> T.many (parser generation lineNumber) |> Parser.map (\list -> LX list Nothing))


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
    Parser.oneOf [ rawString generation lineNumber, plainText generation lineNumber ]


rawString : Int -> Int -> Parser Element
rawString generation lineNumber =
    Parser.succeed (\start source finish -> Text source (Just (Metadata.init generation lineNumber start finish)))
        |= Parser.getOffset
        |= RawString.parser
        |= Parser.getOffset


plainText : Int -> Int -> Parser Element
plainText generation lineNumber =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate XString.isNonLanguageChar
            |> Parser.map (\data -> Text data.content (Just (Metadata.init generation lineNumber data.start data.finish)))
        )


textWithPredicate : (Char -> Bool) -> Int -> Int -> Parser Element
textWithPredicate predicate generation lineNumber =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate predicate
            |> Parser.map (\data -> Text data.content (Just (Metadata.init generation lineNumber data.start data.finish)))
        )


type alias StringData =
    { content : String, start : Int, finish : Int }


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


comma_ =
    Parser.symbol (Parser.Token "," ExpectingComma)


comma =
    T.first comma_ Parser.spaces


rawStringBegin =
    Parser.symbol (Parser.Token "r##" ExpectingRawStringBegin)


rawStringEnd =
    Parser.symbol (Parser.Token "##" ExpectingRawStringEnd)


pipeSymbol =
    Parser.symbol (Parser.Token "|" ExpectingPipe)


leftBracket =
    Parser.symbol (Parser.Token "[" ExpectingLeftBracket)


rightBracket =
    Parser.symbol (Parser.Token "]" ExpectingRightBracket)


{-|

    Use this to parse a string and return information about its location in the source

-}
getChompedString : Int -> Int -> Parser a -> Parser ( String, Maybe Metadata )
getChompedString generation lineNumber parser_ =
    let
        sm first_ last_ source_ =
            let
                src =
                    String.slice first_ last_ source_
            in
            -- ( src, Just { blockOffset = lineNumber, length = last_, offset = 0, generation = generation } )
            -- TODO: is the below correct?
            ( src, Just (Metadata.init generation lineNumber 0 last_) )
    in
    Parser.succeed sm
        |= Parser.getOffset
        |. parser_
        |= Parser.getOffset
        |= Parser.getSource
