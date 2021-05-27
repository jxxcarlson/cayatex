module ParseLoopTests exposing (..)

import Expect
import Parser.Data as Data
import Parser.Driver exposing (parseLoop)
import Parser.Element exposing (..)
import Parser.Getters exposing (getArgs, getBody, strip)
import Test exposing (describe, fuzz, test)


pl str =
    Parser.Driver.parseLoop 0 0 empty str |> .parsed


empty =
    Data.init Data.defaultConfig


suite =
    describe "Parser.Driver"
        [ describe "parseLoop"
            [ test "Various" <|
                \_ ->
                    Expect.equal (2 + 2) 4
            ]
        ]
