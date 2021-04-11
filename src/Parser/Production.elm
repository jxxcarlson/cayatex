module Parser.Production exposing (..)

import Random


type Symbol
    = NTText
    | NTInline
    | NTInlineArgs
    | NTInlineBody
    | NTInlineExpression
    | NTExpression
    | NTBlock
    | NTLX
    | Terminal String
    | TerminalList (List String)


type alias Production =
    { lhs : Symbol, rhs : List Symbol }


type alias Grammar =
    List Production


seed =
    Random.initialSeed 123


nextInt : Random.Seed -> Int -> ( Int, Random.Seed )
nextInt seed_ limit =
    Random.step (Random.int 0 limit) seed_


isTerminal : Symbol -> Bool
isTerminal symbol =
    case symbol of
        Terminal _ ->
            True

        TerminalList _ ->
            True

        _ ->
            False



{-
   InlineExpression ->
           Text String
           | Inline "[" Name Args InlineExpression "]"
           | List InlineExpression
   Args ->
           Empty
           | "|" NonemptyString ("," NonemptyString)* "|"

-}


cyGrammar =
    [ { lhs = NTInlineExpression, rhs = [ Terminal "_x" ] }
    , { lhs = NTInlineExpression, rhs = [ leftBracket, Terminal "_name", NTInlineArgs, NTInlineExpression, rightBracket ] }
    , { lhs = NTInlineArgs, rhs = [ Terminal "" ] }
    , { lhs = NTInlineArgs, rhs = [ pipeSymbol, TerminalList [], pipeSymbol ] }
    ]


nonTerminals : List Symbol -> List Symbol
nonTerminals symbols =
    List.filter (\s -> not (isTerminal s)) symbols


process : List Symbol -> List Symbol
process symbols =
    symbols


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b



--
--expand : Grammar -> Production -> Production
--expand g p =
-- TERMINAL SYMBOLS


pipeSymbol =
    Terminal "|"


leftBracket =
    Terminal "["


rightBracket =
    Terminal "]"
