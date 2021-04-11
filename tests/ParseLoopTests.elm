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

[item Raspberry jam]

[item Sourdough bread]

}
"""


table =
    """{table|
  {row| [Hydrogen, H, 1, 1] }
  {row| [Helium, He, 2, 4]  }
  {row |Lithium, Li, 3, 5]  }
}
"""


suite =
    describe "Parser.Driver"
        [ describe "parseLoop"
            [ test "Various (BAD)" <|
                \_ ->
                    Expect.equal
                        (pl "this [strong is] a test {theorem|many primes}ho ho ho" |> List.map strip)
                        [ Text "this " Nothing, Inline "strong" [] (LX [ Text "is" Nothing ] Nothing) Nothing, Text " a test " Nothing, Block "theorem" [] (Just (LX [ Text "many primes" Nothing ] Nothing)) Nothing, Text "ho ho ho" Nothing ]
            , test "Block arguments" <|
                \_ ->
                    Expect.equal
                        (pl "{theorem [strong c], [italic d], foo|many primes}" |> getArgs)
                        [ Just [ Inline "strong" [] (LX [ Text "c" Nothing ] Nothing) Nothing, Inline "italic" [] (LX [ Text "d" Nothing ] Nothing) Nothing, Text "foo" Nothing ] ]
            , test "Block body (OK)" <|
                \_ ->
                    Expect.equal
                        (pl "{theorem [strong c], [italic d], foo|[strong many] primes}" |> getBody)
                        [ Just (LX [ Inline "strong" [] (LX [ Text "many" Nothing ] Nothing) Nothing, Text " primes" Nothing ] Nothing) ]
            , test "Complex block body (OK)" <|
                \_ ->
                    Expect.equal
                        (pl "{theorem [strong c], [italic d], foo|[strong many] primes}" |> getBody)
                        [ Just (LX [ Inline "strong" [] (LX [ Text "many" Nothing ] Nothing) Nothing, Text " primes" Nothing ] Nothing) ]
            , test "List (OK)" <|
                \_ ->
                    Expect.equal
                        (pl numberedList |> List.map strip)
                        [ Block "numbered-list"
                            []
                            (Just
                                (LX
                                    [ Text "\n\n" Nothing
                                    , Inline "item" [] (LX [ Text "Raspberry jam" Nothing ] Nothing) Nothing
                                    , Text "\n\n" Nothing
                                    , Inline "item" [] (LX [ Text "Sourdough bread" Nothing ] Nothing) Nothing
                                    , Text "\n\n" Nothing
                                    ]
                                    Nothing
                                )
                            )
                            Nothing
                        ]
            ]
        ]
