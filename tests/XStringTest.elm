module XStringTest exposing (..)

import Expect
import Parser exposing (run)
import Parser.XString as X
import Test exposing (describe, fuzz, test)


text =
    X.text |> Parser.map .content


goodTextWithoutEscape =
    X.goodTextWithoutEscape |> Parser.map .content


escapedChar =
    X.escapedChar |> Parser.map .content


suite =
    describe "Parser.Expression"
        [ describe "XString"
            [ test "text without escape" <|
                \_ ->
                    Expect.equal
                        (run text "this is a test")
                        (Ok "this is a test")
            , test "text with escape" <|
                \_ ->
                    Expect.equal
                        (run text "test:\\|")
                        (Ok "test:\\|")
            , test "escapedText" <|
                \_ ->
                    Expect.equal
                        (run text "\\|")
                        (Ok "\\|")
            , test "text" <|
                \_ ->
                    Expect.equal
                        (run X.text "this is a test \\| and so is this!| ho ho ho!!")
                        (Ok { content = "this is a test \\| and so is this!", finish = 33, start = 0 })
            ]
        ]
