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
            "Error. Expecting right bracket after"

        ExpectingLeftBracket ->
            "Error. Expecting left bracket"

        ExpectingPipe ->
            "Error. Expecting pipe symbol"

        _ ->
            "Error in"


type Context
    = CElement
    | CArgs
    | CBody
    | CArgsAndBody
    | TextExpression
