module Parser.Error exposing (heading, Context(..), Problem(..))


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
        ExpectingRightBracket -> "Error. Expecting right bracket in"
        ExpectingLeftBracket -> "Error. Expecting left bracket in"
        ExpectingPipe -> "Error. Expecting piple symbol in"
        _ -> "Error in"



type Context
    = CElement
    | CArgs
    | CBody
    | CArgsAndBody
    | TextExpression
