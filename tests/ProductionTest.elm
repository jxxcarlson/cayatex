module ProductionTest exposing (..)

import Expect
import Parser.Production exposing (..)
import Test exposing (describe, fuzz, test)


suite =
    describe "Parser.Production"
        [ Test.only <|
            describe "helpers"
                [ test "nonTerminals" <|
                    \_ ->
                        Expect.equal
                            (nonTerminals [ NTInlineArgs, Terminal "foo", NTInlineArgs ])
                            [ NTInlineArgs, NTInlineArgs ]
                ]
        ]
