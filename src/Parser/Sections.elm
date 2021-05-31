module Parser.Sections exposing (prefixLength, splitIntoSections, testStr)

import Parser exposing ((|.), (|=))
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

        Just currentLine_ ->
            let
                currentLine =
                    preprocessLine currentLine_
            in
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


preprocessLine : Line -> Line
preprocessLine line =
    if String.left 1 line.content == "#" then
        makeIntoHeading line

    else
        preprocessLine_ line


preprocessLine_ line =
    case String.left 1 line.content of
        "-" ->
            { index = line.index, content = "[item" ++ String.dropLeft 1 line.content ++ "]" }

        "@" ->
            if String.left 5 line.content == "@list" then
                { index = line.index, content = "[list" }

            else if String.left 9 line.content == "@numbered" then
                { index = line.index, content = "[list | s: numbered |" }

            else if String.left 4 line.content == "@end" then
                { index = line.index, content = "  ]" }

            else
                line

        _ ->
            line


makeIntoHeading : Line -> Line
makeIntoHeading line =
    case prefixLength '#' line.content of
        Nothing ->
            line

        Just 1 ->
            { content = "[section1 " ++ String.dropLeft 1 line.content ++ "]", index = line.index }

        Just 2 ->
            { content = "[section2 " ++ String.dropLeft 2 line.content ++ "]", index = line.index }

        Just 3 ->
            { content = "[section3 " ++ String.dropLeft 3 line.content ++ "]", index = line.index }

        Just 4 ->
            { content = "[section4 " ++ String.dropLeft 4 line.content ++ "]", index = line.index }

        Just 5 ->
            { content = "[section4 " ++ String.dropLeft 5 line.content ++ "]", index = line.index }

        _ ->
            line


{-|

    > prefixLength '#' "## abc"
    Just 2

    > prefixLength '#' "#abc"
    Nothing

    > prefixLength '#' "abc"
    Nothing

-}
prefixLength : Char -> String -> Maybe Int
prefixLength char str =
    case Parser.run (prefixLengthParser char) str of
        Ok n ->
            Just n

        Err _ ->
            Nothing


prefixLengthParser : Char -> Parser.Parser Int
prefixLengthParser char =
    (Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> c == char)
            |. Parser.symbol " "
    )
        |> Parser.map (String.length >> (\x -> x - 1))


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
