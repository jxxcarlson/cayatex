module ExpressionTests exposing (..)

import Expect
import Parser.Advanced exposing (run)
import Parser.Expression exposing (..)
import Parser.Getters exposing (getArgs, getBody, strip)
import Parser.Tool as T
import Test exposing (describe, fuzz, test)


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
                        (Ok (Inline "image" [ "height:40", "width:100" ] (LX [ Text "stuff" Nothing ] Nothing) Nothing))
            , test "Inline, complex" <|
                \_ ->
                    Expect.equal
                        (run (T.many (parser 0 0)) "foo bar [strong stuff] ho ho ho" |> Result.map (List.map strip))
                        (Ok [ Text "foo bar " Nothing, Inline "strong" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing, Text " ho ho ho" Nothing ])
            , test "inline (2)" <|
                \_ ->
                    Expect.equal (run (parser 0 0) "[strong [italic stuff]] ho ho ho!" |> Result.map Parser.Getters.strip)
                        (Ok (Inline "strong" [] (LX [ Inline "italic" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] Nothing) Nothing))
            , test "inline (3)" <|
                \_ ->
                    Expect.equal (run (parser 0 0) "[strong |font-size 36, la-di-dah: 79| [italic stuff]] ho ho ho!" |> Result.map Parser.Getters.strip)
                        (Ok (Inline "strong" [ "font-size 36", "la-di-dah: 79" ] (LX [ Inline "italic" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] Nothing) Nothing))
            , test "inlineExpression" <|
                \_ ->
                    Expect.equal (run (parser 0 0) "[strong |font-size 36, la-di-dah: 79| [italic stuff]] ho ho ho!" |> Result.map Parser.Getters.strip)
                        (Ok (Inline "strong" [ "font-size 36", "la-di-dah: 79" ] (LX [ Inline "italic" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] Nothing) Nothing))
            , test "many inlineExpression" <|
                \_ ->
                    Expect.equal (run (T.many (parser 0 0)) "[strong |font-size 36, la-di-dah: 79| [italic stuff]] ho ho [large ho]!" |> Result.map (List.map Parser.Getters.strip))
                        (Ok [ Inline "strong" [ "font-size 36", "la-di-dah: 79" ] (LX [ Inline "italic" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] Nothing) Nothing, Text " ho ho " Nothing, Inline "large" [] (LX [ Text "ho" Nothing ] Nothing) Nothing, Text "!" Nothing ])
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
                        (Ok (Block "yada" [ Inline "strong" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] (Just (LX [ Text "foo bar" Nothing ] Nothing)) Nothing))
            , test "Block with several arguments" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "{yada 7, 8, [strong stuff]|foo bar} mmm" |> Result.map strip)
                        (Ok (Block "yada" [ Text "7" Nothing, Text "8" Nothing, Inline "strong" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing ] (Just (LX [ Text "foo bar" Nothing ] Nothing)) Nothing))
            , test "Nested blocks" <|
                \_ ->
                    Expect.equal
                        (run (parser 1 2) "{indent|\nThis is a test:\n{theorem|\nMany primes!}}" |> Result.map strip)
                        (Ok (Block "indent" [] (Just (LX [ Text "\nThis is a test:\n" Nothing, Block "theorem" [] (Just (LX [ Text "\nMany primes!" Nothing ] Nothing)) Nothing ] Nothing)) Nothing))
            ]
        ]
