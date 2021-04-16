module Parser.Error exposing (Context(..), Problem(..))


type Problem
    = ExpectingComma
    | ExpectingLeftBracket
    | ExpectingRightBracket
    | ExpectingPipe
    | ExpectingEscape
    | EndOfInput
    | UnHandledError


type Context
    = CElement
    | CInline_ String
    | TextExpression
