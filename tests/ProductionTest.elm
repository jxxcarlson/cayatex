module ProductionTest exposing (..)

import Expect
import Parser.Production exposing (..)
import Test exposing (describe, fuzz, test)


suite =
    describe "Parser.Production"
        [ describe "helpers"
            [ test "nonTerminals" <|
                \_ ->
                    Expect.equal
                        (nonTerminals [ GElementArgs, GTerminal "foo", GElementArgs ])
                        [ GElementArgs, GElementArgs ]
            ]
        ]
