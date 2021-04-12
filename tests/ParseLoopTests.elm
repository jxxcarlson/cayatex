module ParseLoopTests exposing (..)

import Expect
import Parser.Driver exposing (parseLoop)
import Parser.Element exposing (..)
import Parser.Getters exposing (getArgs, getBody, strip)
import Test exposing (describe, fuzz, test)


pl str =
    Parser.Driver.parseLoop 0 0 str |> .parsed


suite =
    describe "Parser.Driver"
        [ describe "parseLoop"
            [ test "Various" <|
                \_ ->
                    Expect.equal (2 + 2) 4
            ]
        ]
