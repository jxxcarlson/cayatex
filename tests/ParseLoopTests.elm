module ParseLoopTests exposing (..)

import Expect
import Parser.Driver exposing (parseLoop)
import Parser.Expression exposing (..)
import Parser.Getters exposing (getArgs, getBody, strip)
import Test exposing (describe, fuzz, test)


pl str =
    Parser.Driver.parseLoop 0 0 str |> .parsed


numberedList =
    """|numbered-list|

[item Raspberry jam]

[item Sourdough bread]

|end
"""


table =
    """|table|
  |row| [Hydrogen, H, 1, 1] |end
  |row| [Helium, He, 2, 4]  |end
  |row |Lithium, Li, 3, 5]  |end
|end
"""


suite =
    describe "Parser.Driver"
        [ describe "parseLoop"
            [ test "Various" <|
                \_ ->
                    Expect.equal
                        (pl "this [strong is] a test |theorem |\nmany primes |end ho ho ho" |> List.map strip)
                        [ Text "this " Nothing, Inline "strong" [] (LX [ Text "is" Nothing ] Nothing) Nothing, Text " a test " Nothing, Block "theorem" [] (Just (LX [ Text "many primes " Nothing ] Nothing)) Nothing, Text "ho ho ho" Nothing ]
            , test "Block arguments" <|
                \_ ->
                    Expect.equal
                        (pl "|theorem [strong c], [italic d], foo|\nmany primes|end" |> getArgs)
                        [ Just [ Inline "strong" [] (LX [ Text "c" Nothing ] Nothing) Nothing, Inline "italic" [] (LX [ Text "d" Nothing ] Nothing) Nothing, Text "foo" Nothing ] ]
            , test "Block body" <|
                \_ ->
                    Expect.equal
                        (pl "|theorem [strong c], [italic d], foo|\nmany primes|end" |> getBody)
                        [ Just (LX [ Text "many primes" Nothing ] Nothing) ]
            , test "Complex block body" <|
                \_ ->
                    Expect.equal
                        (pl "|theorem [strong c], [italic d], foo|\n [strong many] primes |end" |> getBody)
                        [ Just (LX [ Text " " Nothing, Inline "strong" [] (LX [ Text "many" Nothing ] Nothing) Nothing, Text " primes " Nothing ] Nothing) ]
            , test "List" <|
                \_ ->
                    Expect.equal
                        (pl numberedList |> List.map strip)
                        [ Block "numbered-list"
                            []
                            (Just
                                (LX
                                    [ Text "\n" Nothing
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
