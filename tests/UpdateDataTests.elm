module UpdateDataTests exposing (..)

import Dict
import Expect
import Parser.Data as Data
import Parser.Driver as Driver
import Parser.Element exposing (..)
import Test exposing (describe, fuzz, test)


pl : String -> List Element
pl str =
    Driver.parseLoop 0 0 str |> .parsed


empty =
    Data.init Data.defaultConfig


suite =
    describe "Parser.Data"
        [ describe "update"
            [ test "two sections" <|
                \_ ->
                    Expect.equal
                        (Data.updateList (pl "[section A]\n\n[section B]") empty |> .counters)
                        (Dict.fromList [ ( "section1", 2 ) ])
            ]
        ]
