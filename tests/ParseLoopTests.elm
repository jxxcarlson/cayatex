module ParseLoopTests exposing (..)

import Expect
import Parser.Driver exposing (parseLoop)
import Parser.Expression exposing (..)
import Parser.Getters exposing (getArgs, getBody, strip)
import Test exposing (describe, fuzz, test)


pl str =
    Parser.Driver.parseLoop 0 0 str |> .parsed


numberedList =
    """
|numbered-list|

[item Raspberry jam]

[item Sourdough bread]

|end
"""


table =
    """
|table|
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
                        (pl "this [strong is] a test |theorem | many primes |end ho ho ho" |> List.map strip)
                        [ Text "this " Nothing
                        , Inline "strong" [] (LX [ Text "is" Nothing ] Nothing) Nothing
                        , Text " a test " Nothing
                        , Block "theorem" [] (Just (Text " many primes " Nothing)) Nothing
                        , Text "ho ho ho" Nothing
                        ]
            , test "Block arguments" <|
                \_ ->
                    Expect.equal
                        (pl "|theorem [strong c], [italic d], foo| many primes |end" |> getArgs)
                        [ Just [ Inline "strong" [] (LX [ Text "c" Nothing ] Nothing) Nothing, Inline "italic" [] (LX [ Text "d" Nothing ] Nothing) Nothing, Text "foo" Nothing ] ]
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
            , Test.skip <|
                test "List" <|
                    \_ ->
                        Expect.equal
                            (pl "|theorem [strong c], [italic d], foo| many primes |end" |> getBody)
                            [ Just (Text " many primes " Nothing) ]
            ]
        ]
