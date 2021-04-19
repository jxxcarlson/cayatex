module DocLoopTests exposing (..)

import Expect
import Parser.Advanced exposing (run)
import Parser.Document as Document exposing (BlockStatus(..), State, Step(..), applyNextState, getParseResult, nextState)
import Parser.Element exposing (..)
import Parser.Tool as T
import Test exposing (describe, fuzz, test)


init str =
    Document.init 0 (String.lines str)


suite =
    describe "Parser.Document"
        [ describe "nextState, from Start"
            [ test "init" <|
                \_ ->
                    Expect.equal
                        (init "")
                        { blockContents = [], blockLevel = 0, blockType = Start, generation = 0, input = [ "" ], lineNumber = 0, output = [] }
            , test "start-blank" <|
                \_ ->
                    Expect.equal
                        (nextState <| init "")
                        (Loop { blockContents = [], blockLevel = 0, blockType = Start, generation = 0, input = [], lineNumber = 0, output = [] })
            , test "start-begin-element" <|
                \_ ->
                    Expect.equal
                        (nextState <| { blockContents = [], blockLevel = 0, blockType = Start, generation = 0, input = [ "[strong stuff]" ], lineNumber = 0, output = [] })
                        (Loop { blockContents = [ "[strong stuff]" ], blockLevel = 0, blockType = InElementBlock, generation = 0, input = [], lineNumber = 0, output = [] })
            , Test.skip <|
                test "start-end-element" <|
                    \_ ->
                        Expect.equal
                            (nextState <| { blockContents = [], blockLevel = 0, blockType = Start, generation = 0, input = [ "strong stuff]" ], lineNumber = 0, output = [] })
                            (Loop { blockContents = [ "Error (missingLeftBracket): strong  _RIGHTBRACKET" ], blockLevel = 0, blockType = InTextBlock, generation = 0, input = [], lineNumber = 0, output = [] })
            ]
        , describe "loop"
            [ test "simple element" <|
                \_ ->
                    let
                        src =
                            { blockContents = [], blockLevel = 0, blockType = Start, generation = 0, input = [ "[strong stuff]", "" ], lineNumber = 0, output = [] }
                    in
                    Expect.equal
                        (applyNextState (applyNextState (nextState src)) |> getParseResult)
                        [ [ Element "strong" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing, Text "\n\n" Nothing ] ]
            , test "element and text, different lines" <|
                \_ ->
                    let
                        src =
                            { blockContents = [], blockLevel = 0, blockType = Start, generation = 0, input = [ "[strong stuff]", "is OK" ], lineNumber = 0, output = [] }
                    in
                    Expect.equal
                        (applyNextState (applyNextState (nextState src)) |> getParseResult)
                        [ [ Element "strong" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing, Text "\nis OK" Nothing ] ]
            , test "two elements in succession, different lines" <|
                \_ ->
                    let
                        src =
                            { blockContents = [], blockLevel = 0, blockType = Start, generation = 0, input = [ "[strong stuff]", "[italic is OK]" ], lineNumber = 0, output = [] }
                    in
                    Expect.equal
                        (applyNextState (applyNextState (nextState src)) |> getParseResult)
                        [ [ Element "strong" [] (LX [ Text "stuff" Nothing ] Nothing) Nothing, Text "\n" Nothing, Element "italic" [] (LX [ Text "is OK" Nothing ] Nothing) Nothing ] ]
            , test "nested elements, different lines" <|
                \_ ->
                    let
                        src =
                            { blockContents = [], blockLevel = 0, blockType = Start, generation = 0, input = [ "[strong it", "[italic is OK]]" ], lineNumber = 0, output = [] }
                    in
                    Expect.equal
                        (applyNextState (applyNextState (nextState src)) |> getParseResult)
                        [ [ Element "strong" [] (LX [ Text "it\n" Nothing, Element "italic" [] (LX [ Text "is OK" Nothing ] Nothing) Nothing ] Nothing) Nothing ] ]
            ]
        ]
