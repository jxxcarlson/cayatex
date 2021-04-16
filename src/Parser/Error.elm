module Parser.Error exposing (Context(..), Problem(..))


type Problem
    = ExpectingComma
    | ExpectingLeftBracket
    | ExpectingRightBracket
    | ExpectingPipe
    | ExpectingEscape
    | ExpectingLanguageChar
    | EndOfInput
    | UnHandledError


type Context
    = CElement
    | CInline_ String
    | DisplayMathContext
    | CManyExpression
    | TextExpression
