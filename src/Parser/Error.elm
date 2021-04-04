module Parser.Error exposing (Problem(..), Context(..))


type Problem
    = ExpectingToken String
    | ExpectingLeadingDoubleDollarSign
    | EndOfInput
    | UnHandledError


type Context
    = InlineFunction
    | DisplayMathContext