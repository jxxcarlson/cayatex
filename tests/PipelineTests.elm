module PipelineTests exposing (suite)

import Expect
import Parser.Advanced exposing (run)
import Parser.Document as Document exposing (BlockStatus(..), State, Step(..), applyNextState, getParseResult, nextState)
import Parser.Element exposing (..)
import Parser.Sections
import Parser.Tool as T
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
                ]
        ]
