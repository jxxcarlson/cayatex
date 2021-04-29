module UpdateDataTests exposing (..)

import Dict
import Expect
import Parser.Data as Data exposing (Data)
import Parser.Driver as Driver
import Parser.Element as Element exposing (Element)
import Test exposing (describe, fuzz, test)


pl : String -> List Element
pl str =
    Driver.parseLoop 0 0 str |> .parsed


empty =
    Data.init Data.defaultConfig


suite =
    describe "Parser.Data"
        [ describe "update"
            [ test "two sections from element" <|
                \_ ->
                    let
                        updateCounters : Element -> Data
                        updateCounters e =
                            Data.update e empty
                    in
                    Expect.equal
                        (Result.map (Element.makeList >> updateCounters >> .counters) (Element.parseList 0 0 "[section A]\n\n[section B]"))
                        (Ok <| Dict.fromList [ ( "section1", 2 ) ])
            ]
        , test "two sections from element list" <|
            \_ ->
                Expect.equal
                    (Data.updateList (pl "[section A]\n\n[section B]") empty |> .counters)
                    (Dict.fromList [ ( "section1", 2 ) ])
        ]
