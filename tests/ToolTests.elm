module ToolTests exposing (suite)

import Expect
import Parser exposing (Parser, int, run, symbol)
import Parser.ToolSimple exposing (..)
import Test exposing (describe, fuzz, test)


bracketedInt : Parser Int
bracketedInt =
    first (between (symbol "[") int (symbol "]")) Parser.spaces


bracketed : Parser b -> Parser b
bracketed p =
    between (symbol "[") p (symbol "]")


{-| The parser line1 looks like it will (1) parse a line
(2) parse many lines using (many line1). However, line1
does not consume the trailing '\\n', and so (many line1)
fails to work as intended. See parser line for a solution.
-}
line1 : Parser String
line1 =
    text Char.isAlpha (\c -> c /= '\n') |> Parser.map .content


line : Parser String
line =
    first line1 (Parser.oneOf [ Parser.symbol "\n", Parser.end ])


suite =
    describe "Parser.ToolSimple"
        [ describe "functions"
            [ test "line" <|
                \_ ->
                    Expect.equal
                        (run line1 "one two\nthree four\n")
                        (Ok "one two")
            , test "lines (1)" <|
                \_ ->
                    Expect.equal
                        (run (many line1) "one\ntwo\nthree")
                        (Ok [ "one" ])
            , test "lines (2)" <|
                \_ ->
                    Expect.equal
                        (run (many line) "one\ntwo\nthree")
                        (Ok [ "one", "two", "three" ])
            , test "many" <|
                \_ ->
                    Expect.equal
                        (run (many bracketedInt) "[1] [2]")
                        (Ok [ 1, 2 ])
            , test "bracketed" <|
                \_ -> Expect.equal (run (bracketed (many bracketedInt)) "[[1] [2]]") (Ok [ 1, 2 ])
            ]
        ]
