module Parser.Error exposing (Context(..), Problem(..))


type Problem
    = ExpectingToken String
    | ExpectingLeadingDoubleDollarSign
    | EndOfInput
    | UnHandledError


type Context
    = InlineFunction
    | DisplayMathContext
