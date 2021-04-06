module ExpressionTests exposing (..)

import Expect
import Parser.Advanced exposing (run)
import Parser.Expression exposing (..)
import Parser.Getters exposing (getArgs, getBody, strip)
import Test exposing (describe, fuzz, test)


suite =
    describe "Parser.Expression"
        [ describe "parser"
            [ test "Text" <|
                \_ ->
                    Expect.equal
                        (run (parser 0 0) "this is a test")
                        (Ok (Text "this is a test" (Just { blockOffset = 0, generation = 0, length = 14, offset = 0 })))
            , test "Inline" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "[image |height:40,width:100| stuff]" |> Result.map strip)
                        (Ok (Inline "image" [ "height:40", "width:100" ] (Text "stuff" Nothing) Nothing))
            , test "Block" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "|yada| foo bar |end mmm" |> Result.map strip)
                        (Ok (Block "yada" [] (Just (Text " foo bar " Nothing)) Nothing))
            , test "Block with argument" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "|yada [strong stuff]| foo bar |end mmm" |> Result.map strip)
                        (Ok (Block "yada" [ Inline "strong" [] (Text "stuff" Nothing) Nothing ] (Just (Text " foo bar " Nothing)) Nothing))
            ]
        ]
