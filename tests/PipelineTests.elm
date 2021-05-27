module PipelineTests exposing (suite)

import Expect
import Parser.Document
import Parser.Element exposing (..)
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
        [ Test.only <|
            describe "Pipeline Elements"
                [ test "splitIntoSections & get prelude" <|
                    \_ ->
                        Parser.Sections.splitIntoSections testDoc |> .prelude |> Expect.equal [ "one", "two", "three", "" ]
                , test "splitIntoSections & get sections" <|
                    \_ ->
                        Parser.Sections.splitIntoSections testDoc
                            |> .sections
                            |> Expect.equal [ [ "# Intro", "four", "five", "", "six", "seven", "" ], [ "## Body", "eight", "nine", "" ] ]
                , test "Parser.Document.process (the top level function)" <|
                    \_ ->
                        Parser.Document.process 0 (String.lines "test\nthis stuff")
                            |> Parser.Document.toParsed
                            |> Expect.equal [ [ Text "test\nthis stuff" (Just { blockOffset = 0, generation = 0, label = "", length = 15, offset = 0 }) ] ]
                ]
        ]
