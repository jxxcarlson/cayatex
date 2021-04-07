module ToolTests exposing (..)

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


suite =
    describe "Parser.ToolSimple"
        [ describe "functions"
            [ test "many" <|
                \_ ->
                    Expect.equal
                        (run (many bracketedInt) "[1] [2]")
                        (Ok [ 1, 2 ])
            , test "bracketed" <|
                \_ -> Expect.equal (run (bracketed (many bracketedInt)) "[[1] [2]]") (Ok [ 1, 2 ])
            ]
        ]
