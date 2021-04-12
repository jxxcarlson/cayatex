module ParseLoopTests exposing (..)

import Expect
import Parser.Driver exposing (parseLoop)
import Parser.Expression exposing (..)
import Parser.Getters exposing (getArgs, getBody, strip)
import Test exposing (describe, fuzz, test)


pl str =
    Parser.Driver.parseLoop 0 0 str |> .parsed


numberedList =
    """{numbered-list|

{item| Raspberry jam}

{item| Sourdough bread}

}
"""


table =
    """{table|
{row| Hydrogen, 1, 1}
{row| Helium, 2, 4}
{row| Lithium, 3, 6}
}
"""


csv =
    """{csv|
Hydrogen, 1, 1
Helium, 2, 4
Lithium, 3, 6
}
"""


suite =
    describe "Parser.Driver"
        [ describe "parseLoop"
            [ test "Various" <|
                \_ ->
                    Expect.equal
                        (pl "this [strong is] a test {theorem|many primes}ho ho ho" |> List.map strip)
                        [ Text "this " Nothing, Inline "strong" [] (LX [ Text "is" Nothing ] Nothing) Nothing, Text " a test " Nothing, Block "theorem" [] (Just (LX [ Text "many primes" Nothing ] Nothing)) Nothing, Text "ho ho ho" Nothing ]
            , test "Block arguments" <|
                \_ ->
                    Expect.equal
                        (pl "{theorem [strong c], [italic d], foo|many primes}" |> getArgs)
                        [ Just [ Inline "strong" [] (LX [ Text "c" Nothing ] Nothing) Nothing, Inline "italic" [] (LX [ Text "d" Nothing ] Nothing) Nothing, Text "foo" Nothing ] ]
            , test "Block body" <|
                \_ ->
                    Expect.equal
                        (pl "{theorem [strong c], [italic d], foo|[strong many] primes}" |> getBody)
                        [ Just (LX [ Inline "strong" [] (LX [ Text "many" Nothing ] Nothing) Nothing, Text " primes" Nothing ] Nothing) ]
            , test "Complex block body" <|
                \_ ->
                    Expect.equal
                        (pl "{theorem [strong c], [italic d], foo|[strong many] primes}" |> getBody)
                        [ Just (LX [ Inline "strong" [] (LX [ Text "many" Nothing ] Nothing) Nothing, Text " primes" Nothing ] Nothing) ]
            , test "List" <|
                \_ ->
                    Expect.equal
                        (pl numberedList |> List.map strip)
                        [ Block "numbered-list"
                            []
                            (Just
                                (LX
                                    [ Text "\n\n" Nothing
                                    , Block "item" [] (Just (LX [ Text " Raspberry jam" Nothing ] Nothing)) Nothing
                                    , Block "item" [] (Just (LX [ Text " Sourdough bread" Nothing ] Nothing)) Nothing
                                    ]
                                    Nothing
                                )
                            )
                            Nothing
                        ]
            , test "table" <|
                \_ ->
                    Expect.equal
                        (pl table |> List.map strip)
                        [ Block "table"
                            []
                            (Just
                                (LX
                                    [ Text "\n" Nothing
                                    , Block "row" [] (Just (LX [ Text " Hydrogen, 1, 1" Nothing ] Nothing)) Nothing
                                    , Block "row" [] (Just (LX [ Text " Helium, 2, 4" Nothing ] Nothing)) Nothing
                                    , Block "row" [] (Just (LX [ Text " Lithium, 3, 6" Nothing ] Nothing)) Nothing
                                    ]
                                    Nothing
                                )
                            )
                            Nothing
                        ]
            , test
                "csv"
              <|
                \_ ->
                    Expect.equal
                        (pl csv |> List.map strip)
                        [ Block "csv"
                            []
                            (Just
                                (LX [ Text "\nHydrogen, 1, 1\nHelium, 2, 4\nLithium, 3, 6\n" Nothing ] Nothing)
                            )
                            Nothing
                        ]
            , test
                "text + csv"
              <|
                \_ ->
                    Expect.equal
                        (pl ("Below is a CSV table.\nIt will be processed further in the rendering step\n" ++ csv) |> List.map strip)
                        [ Text "Below is a CSV table.\nIt will be processed further in the rendering step\n" Nothing
                        , Block "csv" [] (Just (LX [ Text "\nHydrogen, 1, 1\nHelium, 2, 4\nLithium, 3, 6\n" Nothing ] Nothing)) Nothing
                        ]
            , test
                "block with math in body"
              <|
                \_ ->
                    Expect.equal
                        (pl "{theorem Euclid | There are infinitely many primes [math p \\not\\equiv 1 \\mod 4] }" |> List.map strip)
                        [ Block "theorem" [ Text "Euclid " Nothing ] (Just (LX [ Text " There are infinitely many primes " Nothing, Inline "math" [] (LX [ Text "p \\not\\equiv 1 \\mod 4" Nothing ] Nothing) Nothing, Text " " Nothing ] Nothing)) Nothing ]
            ]
        ]
