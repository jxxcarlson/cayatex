module Parser.Error exposing (Context(..), Problem(..), heading)


type Problem
    = ExpectingComma
    | ExpectingLeftBracket
    | ExpectingRightBracket
    | ExpectingPipe
    | ExpectingEscape
    | EndOfInput
    | ExpectingRawStringBegin
    | ExpectingRawStringEnd
    | ExpectingRawPrefix
    | UnHandledError Int


heading : Problem -> String
heading problem =
    case problem of
        ExpectingRightBracket ->
            "Expecting right bracket in"

        ExpectingLeftBracket ->
            "Expecting left bracket in"

        ExpectingPipe ->
            "Expecting pipe symbol in"

        _ ->
            "Error in"


type Context
    = CElement
    | CArgs
    | CBody
    | CArgsAndBody
    | TextExpression
