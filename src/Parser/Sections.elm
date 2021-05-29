module Parser.Sections exposing (splitIntoSections, testStr)

import Parser.Advanced as Parser
import Parser.Types exposing (..)


testStr =
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


type LineType
    = Marked
    | UnMarked


type ProcessStatus
    = InPrelude
    | InSection


splitIntoSections : String -> { prelude : Lines, sections : List Section }
splitIntoSections str =
    loop (initialSplliterState str) nextSplitterState


type alias SplitterState =
    { prelude : Lines, sections : List Section, accum : Lines, lines : Lines, status : ProcessStatus }


split : String -> List Line
split str =
    str
        |> String.lines
        |> List.indexedMap (\k s -> { index = k, content = s })


initialSplliterState : String -> SplitterState
initialSplliterState str =
    { prelude = [], sections = [], accum = [], lines = split str, status = InPrelude }


nextSplitterState : SplitterState -> Step SplitterState Document
nextSplitterState state =
    case List.head state.lines of
        Nothing ->
            Done { prelude = List.reverse state.prelude, sections = List.reverse <| List.reverse state.accum :: state.sections }

        Just currentLine ->
            case ( lineType currentLine, state.status ) of
                ( UnMarked, InPrelude ) ->
                    -- continue prelude
                    Loop { state | lines = List.drop 1 state.lines, accum = currentLine :: state.accum }

                ( Marked, InPrelude ) ->
                    -- start section
                    Loop { state | lines = List.drop 1 state.lines, accum = [ makeIntoHeading currentLine ], prelude = state.accum, status = InSection }

                ( UnMarked, InSection ) ->
                    -- continue section
                    Loop { state | lines = List.drop 1 state.lines, accum = currentLine :: state.accum }

                ( Marked, InSection ) ->
                    -- start section
                    Loop { state | lines = List.drop 1 state.lines, accum = [ makeIntoHeading currentLine ], sections = List.reverse state.accum :: state.sections }


makeIntoHeading : Line -> Line
makeIntoHeading line =
    { content = "[section1 " ++ String.dropLeft 1 line.content ++ "]", index = line.index }


lineType : Line -> LineType
lineType line =
    if String.left 1 line.content == "#" then
        Marked

    else
        UnMarked


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b
