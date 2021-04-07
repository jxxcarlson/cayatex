module ToolTests exposing (..)

import Expect
import Parser exposing (Parser, int, run, symbol)
import Parser.ToolSimple exposing (..)
import Test exposing (describe, fuzz, test)


int_ : Parser Int
int_ =
    first (between (symbol "[") int (symbol "]")) Parser.spaces


suite =
    describe "Parser.Driver"
        [ describe "parseLoop"
            [ test "Various" <|
                \_ ->
                    Expect.equal
                        (run (many int_) "[1] [2]")
                        (Ok [ 1, 2 ])
            ]
        ]
