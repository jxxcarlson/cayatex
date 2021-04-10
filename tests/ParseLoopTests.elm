module ParseLoopTests exposing (..)

import Expect
import Parser.Driver exposing (parseLoop)
import Parser.Expression exposing (..)
import Parser.Getters exposing (getArgs, getBody, strip)
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
                        , Inline "strong" [] (Text "is" Nothing) Nothing
                        , Text " a test " Nothing
                        , Block "theorem" [] (Just (Text " many primes " Nothing)) Nothing
                        , Text "ho ho ho" Nothing
                        ]
            , test "Block arguments" <|
                \_ ->
                    Expect.equal
                        (pl "|theorem [strong c], [italic d], foo| many primes |end" |> getArgs)
                        [ Just [ Inline "strong" [] (Text "c" Nothing) Nothing, Inline "italic" [] (Text "d" Nothing) Nothing, Text "foo" Nothing ] ]
            , test "Block body" <|
                \_ ->
                    Expect.equal
                        (pl "|theorem [strong c], [italic d], foo| many primes |end" |> getBody)
                        [ Just (Text " many primes " Nothing) ]
            , Test.skip <|
                test "Complex block body" <|
                    \_ ->
                        Expect.equal
                            (pl "|theorem [strong c], [italic d], foo| [strong many] primes |end" |> getBody)
                            [ Just (Text " [strong many] primes " Nothing) ]
            ]
        ]
