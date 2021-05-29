module PipelineTests exposing (suite)

import Expect
import Parser.Element exposing (..)
import Parser.Lines
import Parser.Sections
import Test exposing (describe, fuzz, test)


testDoc =
    """one
two
three

# Intro
four
five

six
seven

## Body
eight
nine
"""


suite =
    describe "Parser Pipeline"
        [ describe "Pipeline Elements"
            [ test "splitIntoSections & get prelude" <|
                \_ ->
                    Parser.Sections.splitIntoSections testDoc
                        |> .prelude
                        |> Expect.equal [ { content = "one", index = 0 }, { content = "two", index = 1 }, { content = "three", index = 2 }, { content = "", index = 3 } ]
            , test "splitIntoSections & get sections" <|
                \_ ->
                    Parser.Sections.splitIntoSections testDoc
                        |> .sections
                        |> Expect.equal
                            [ [ { content = "# Intro", index = 4 }, { content = "four", index = 5 }, { content = "five", index = 6 }, { content = "", index = 7 }, { content = "six", index = 8 }, { content = "seven", index = 9 }, { content = "", index = 10 } ]
                            , [ { content = "## Body", index = 11 }, { content = "eight", index = 12 }, { content = "nine", index = 13 }, { content = "", index = 14 } ]
                            ]
            , test "Parser.Lines.process (the top level function)" <|
                \_ ->
                    Parser.Lines.process 0 (String.lines "test\nthis stuff")
                        |> Parser.Lines.toParsed
                        |> Expect.equal [ [ Text "test\nthis stuff" (Just { blockOffset = 0, generation = 0, label = "", length = 15, offset = 0 }) ] ]
            ]
        ]
