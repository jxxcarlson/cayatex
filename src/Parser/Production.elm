module Parser.Production exposing (..)

import List.Extra
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


grammar =
    [ { lhs = NTInlineExpression, rhs = [ Terminal "_x" ] }
    , { lhs = NTInlineExpression, rhs = [ leftBracket, Terminal "_name", NTInlineArgs, NTInlineExpression, rightBracket ] }
    , { lhs = NTInlineArgs, rhs = [ Terminal "" ] }
    , { lhs = NTInlineArgs, rhs = [ pipeSymbol, TerminalList [], pipeSymbol ] }
    ]


nonTerminals : List Symbol -> List Symbol
nonTerminals symbols =
    List.filter (\s -> not (isTerminal s)) symbols


generate : Int -> List Symbol -> String
generate k symbols =
    process k symbols |> List.map render |> String.join " "


process : Int -> List Symbol -> List Symbol
process k symbols =
    let
        initialState =
            { symbols = symbols, seed = Random.initialSeed k, count = 0 }
    in
    loop initialState nextState


type Step state a
    = Loop state
    | Done a


type alias State =
    { symbols : List Symbol, seed : Random.Seed, count : Int }


nextState : State -> Step State (List Symbol)
nextState state =
    let
        nt =
            nonTerminals state.symbols
    in
    if state.count > 10 then
        Done state.symbols

    else if nt == [] then
        Done state.symbols

    else
        let
            ( k1, s1 ) =
                nextInt state.seed (List.length state.symbols - 1)

            symbol1 =
                List.Extra.getAt k1 state.symbols

            ( _, s2, newSymbols ) =
                case symbol1 of
                    Nothing ->
                        ( k1, s1, state.symbols )

                    Just symbol2 ->
                        if isTerminal symbol2 then
                            ( k1, s1, state.symbols )

                        else
                            let
                                consequents =
                                    matches symbol2 grammar
                            in
                            if consequents == [] then
                                ( k1, s1, state.symbols )

                            else
                                let
                                    ( k_, s_ ) =
                                        nextInt s1 (List.length consequents - 1)

                                    replacement_ =
                                        List.Extra.getAt k_ consequents
                                in
                                case replacement_ of
                                    Nothing ->
                                        ( k_, s_, state.symbols )

                                    Just rhs ->
                                        ( k_, s_, replace k1 rhs state.symbols )
        in
        Loop { state | seed = s2, count = state.count + 1, symbols = newSymbols }


matches : a -> List { lhs : a, rhs : b } -> List b
matches a rules =
    List.filter (\rule -> rule.lhs == a) rules
        |> List.map .rhs


render : Symbol -> String
render symbol =
    case symbol of
        Terminal s ->
            case s of
                "_name" ->
                    "strong"

                "_x" ->
                    "foo"

                _ ->
                    s

        TerminalList [] ->
            "1, 2, 3"

        _ ->
            ""


{-|

    > replace 2 [15, 16] [0,1, 2, 3, 4]
    [0,1,15,16,3,4]

-}
replace : Int -> List a -> List a -> List a
replace k replacement targetList =
    let
        ( prefix, suffix ) =
            List.Extra.splitAt k targetList

        newSuffix =
            replacement ++ List.drop 1 suffix
    in
    prefix ++ newSuffix


loop : state -> (state -> Step state a) -> a
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

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
