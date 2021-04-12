module ExpressionTests exposing (..)

import Expect
import Parser.Advanced exposing (run)
import Parser.Expression exposing (..)
import Parser.Getters exposing (getArgs, getBody, strip)
import Parser.Tool as T
import Test exposing (describe, fuzz, test)


numberedList =
    """[numbered-list 

[item Raspberry jam]

[item Sourdough bread]

]
"""


table =
    """[table

[row Hydrogen, 1, 1]

[row Helium, 2, 4]

[row Lithium, 3, 6]

]
"""


csv =
    """[csv
Hydrogen, 1, 1
Helium, 2, 4
Lithium, 3, 6
]
"""


verbatim =
    """[verbatim
This is a test:
  indented 2

    indented 4
]
"""


p str =
    run (parser 1 2) str |> Result.map strip


suite =
    describe "Parser.Expression"
        [ describe "inline"
            [ test "Text" <|
                \_ ->
                    Expect.equal
                        (run (parser 0 0) "this is a test")
                        (Ok (Text "this is a test" (Just { blockOffset = 0, generation = 0, length = 14, offset = 0 })))
            , test "Inline" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "[image |height:40, width:100| stuff]" |> Result.map strip)
                        (Ok (Element "image" [ "height:40", "width:100" ] (LX [ Text "stuff" Nothing ] Nothing) Nothing))
            , test "Inline, complex" <|
                \_ ->
                    Expect.equal
                        (run (T.many (parser 0 0)) "foo bar [strong stuff] ho ho ho" |> Result.map (List.map strip))
                        (Ok [ Text "foo bar " Nothing, Element "strong" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing, Text " ho ho ho" Nothing ])
            , test "inline (2)" <|
                \_ ->
                    Expect.equal (run (parser 0 0) "[strong [italic stuff]] ho ho ho!" |> Result.map Parser.Getters.strip)
                        (Ok (Element "strong" [] (LX [ Element "italic" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] Nothing) Nothing))
            , test "inline (3)" <|
                \_ ->
                    Expect.equal (run (parser 0 0) "[strong |font-size 36, la-di-dah: 79| [italic stuff]] ho ho ho!" |> Result.map Parser.Getters.strip)
                        (Ok (Element "strong" [ "font-size 36", "la-di-dah: 79" ] (LX [ Element "italic" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] Nothing) Nothing))
            , test "inlineExpression" <|
                \_ ->
                    Expect.equal (run (parser 0 0) "[strong |font-size 36, la-di-dah: 79| [italic stuff]] ho ho ho!" |> Result.map Parser.Getters.strip)
                        (Ok (Element "strong" [ "font-size 36", "la-di-dah: 79" ] (LX [ Element "italic" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] Nothing) Nothing))
            , test "many inlineExpression" <|
                \_ ->
                    Expect.equal (run (T.many (parser 0 0)) "[strong |font-size 36, la-di-dah: 79| [italic stuff]] ho ho [large ho]!" |> Result.map (List.map Parser.Getters.strip))
                        (Ok [ Element "strong" [ "font-size 36", "la-di-dah: 79" ] (LX [ Element "italic" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] Nothing) Nothing, Text " ho ho " Nothing, Element "large" [] (LX [ Text "ho" Nothing ] Nothing) Nothing, Text "!" Nothing ])
            ]
        , describe "inline-extended" <|
            [ test "theorem" <|
                \_ ->
                    Expect.equal
                        (p "[theorem |title Euclid| There are infinitely many primes [math p \\equiv 1 \\mod 4]]")
                        (Ok (Element "theorem" [ "title Euclid" ] (LX [ Text "There are infinitely many primes " Nothing, Element "math" [] (LX [ Text "p \\equiv 1 \\mod 4" Nothing ] Nothing) Nothing ] Nothing) Nothing))
            , test "numbered-list" <|
                \_ ->
                    Expect.equal
                        (p numberedList)
                        (Ok
                            (Element "numbered-list"
                                []
                                (LX
                                    [ Element "item" [] (LX [ Text "Raspberry jam" Nothing ] Nothing) Nothing
                                    , Text "\n\n" Nothing
                                    , Element "item" [] (LX [ Text "Sourdough bread" Nothing ] Nothing) Nothing
                                    , Text "\n\n" Nothing
                                    ]
                                    Nothing
                                )
                                Nothing
                            )
                        )
            , test "table" <|
                \_ ->
                    Expect.equal
                        (p table)
                        (Ok
                            (Element "table"
                                []
                                (LX
                                    [ Element "row" [] (LX [ Text "Hydrogen, 1, 1" Nothing ] Nothing) Nothing
                                    , Text "\n\n" Nothing
                                    , Element "row" [] (LX [ Text "Helium, 2, 4" Nothing ] Nothing) Nothing
                                    , Text "\n\n" Nothing
                                    , Element "row" [] (LX [ Text "Lithium, 3, 6" Nothing ] Nothing) Nothing
                                    , Text "\n\n" Nothing
                                    ]
                                    Nothing
                                )
                                Nothing
                            )
                        )
            , test "csv" <|
                \_ ->
                    Expect.equal
                        (p csv)
                        (Ok (Element "csv" [] (LX [ Text "Hydrogen, 1, 1\nHelium, 2, 4\nLithium, 3, 6\n" Nothing ] Nothing) Nothing))
            , test "verbatim" <|
                \_ ->
                    Expect.equal
                        (p verbatim)
                        (Ok (Element "verbatim" [] (LX [ Text "This is a test:\n  indented 2\n\n    indented 4\n" Nothing ] Nothing) Nothing))
            ]
        , describe "block" <|
            [ test "Block" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "{yada|foo bar}mmm" |> Result.map strip)
                        (Ok (Block "yada" [] (Just (LX [ Text "foo bar" Nothing ] Nothing)) Nothing))
            , test "Block with argument" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "{yada [strong stuff]|foo bar}mmm" |> Result.map strip)
                        (Ok (Block "yada" [ Element "strong" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] (Just (LX [ Text "foo bar" Nothing ] Nothing)) Nothing))
            , test "Block with several arguments" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "{yada 7, 8, [strong stuff]|foo bar} mmm" |> Result.map strip)
                        (Ok (Block "yada" [ Text "7" Nothing, Text "8" Nothing, Element "strong" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] (Just (LX [ Text "foo bar" Nothing ] Nothing)) Nothing))
            , test "Nested blocks" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "{indent|\nThis is a test:\n{theorem|\nMany primes!}}" |> Result.map strip)
                        (Ok (Block "indent" [] (Just (LX [ Text "\nThis is a test:\n" Nothing, Block "theorem" [] (Just (LX [ Text "\nMany primes!" Nothing ] Nothing)) Nothing ] Nothing)) Nothing))
            ]
        ]
