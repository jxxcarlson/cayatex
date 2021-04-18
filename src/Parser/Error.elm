module Parser.Error exposing (Context(..), Problem(..))


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


type Context
    = CElement
    | CArgs
    | CBody
    | CArgsAndBody
    | TextExpression
