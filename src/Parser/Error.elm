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
            "Missing right bracket?"

        ExpectingLeftBracket ->
            "Missing left bracket?"

        ExpectingPipe ->
            "Missing pipe symbol?"

        _ ->
            "Error in"


type Context
    = CElement
    | CArgs
    | CBody
    | CArgsAndBody
    | TextExpression
