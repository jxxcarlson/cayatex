module Parser.Error exposing (Context(..), Problem(..))


type Problem
    = ExpectingToken String
    | ExpectingEscape
    | ExpectingLanguageChar
    | EndOfInput
    | UnHandledError


type Context
    = CInline
    | CInline_ String
    | DisplayMathContext
    | CManyExpression
    | TextExpression
