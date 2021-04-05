module ParseLoopTests exposing (..)

import Expect
import Parser.Driver exposing (parseLoop)
import Parser.Expression exposing (..)
import Test exposing (describe, fuzz, test)


pl str =
    Parser.Driver.parseLoop 0 0 str |> .parsed


suite =
    describe "Parser.Driver"
        [ describe "parseLoop"
            [ test "Various" <|
                \_ ->
                    Expect.equal
                        (pl "this [strong is] a test |theorem | many primes |end ho ho ho" |> List.map strip)
                        [ Text "this " Nothing
                        , Inline "strong" [] "is" Nothing
                        , Text " a test " Nothing
                        , Block "theorem" [] (Just (Text " many primes " Nothing)) Nothing
                        , Text "ho ho ho" Nothing
                        ]
            , Test.only <| test "Block arguments" <|
                \_ ->
                    Expect.equal
                        (pl "|theorem [strong c], [italic d], foo| many primes |end" |> getArgs)
                        [ Just
                            [ Inline "strong" [] "c" Nothing
                            , Inline "italic" [] "d" Nothing
                            , Text "foo" Nothing
                            ]
                        ]
            , test "Block body" <|
                \_ ->
                    Expect.equal
                        (pl "|theorem [strong c], [italic d], foo| many primes |end" |> getBody)
                        [Just (Text (" many primes ") Nothing)]

            , test "Complex block body" <|
                \_ ->
                    Expect.equal
                        (pl "|theorem [strong c], [italic d], foo| [strong many] primes |end" |> getBody)
                        [Just (Text " [strong many] primes " Nothing)]            ]
        ]
