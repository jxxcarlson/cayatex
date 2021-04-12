module Parser.Production exposing (..)

import List.Extra
import Random


type Symbol
    = GText
    | GPrmitiveElement
    | GElementArgs
    | GElementBody
    | GElement
    | GLX
    | GTerminal String
    | GTerminalList (List String)


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
        GTerminal _ ->
            True

        GTerminalList _ ->
            True

        _ ->
            False


grammar =
    [ { lhs = GElement, rhs = [ GTerminal "_x" ] }
    , { lhs = GElement, rhs = [ leftBracket, GTerminal "_name", GElementArgs, GElement, rightBracket ] }
    , { lhs = GElementArgs, rhs = [ GTerminal "" ] }
    , { lhs = GElementArgs, rhs = [ pipeSymbol, GTerminalList [], pipeSymbol ] }
    ]


nonTerminals : List Symbol -> List Symbol
nonTerminals symbols =
    List.filter (\s -> not (isTerminal s)) symbols


{-|

    > generate 33 [GElement]
    > "[italic [green mountain ]]"

-}
generate : Int -> List Symbol -> String
generate k symbols =
    process k symbols
        |> (\symbols_ -> ( Random.initialSeed k, symbols_ ))
        |> mapWithState render2
        |> Tuple.second
        |> List.reverse
        |> String.join ""


mapWithState : (s -> a -> ( s, b )) -> ( s, List a ) -> ( s, List b )
mapWithState f ( state, list ) =
    let
        folder : a -> ( s, List b ) -> ( s, List b )
        folder item ( state_, list_ ) =
            let
                ( newState_, item_ ) =
                    f state_ item
            in
            ( newState_, item_ :: list_ )
    in
    List.foldl folder ( state, [] ) list


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


render2 : Random.Seed -> Symbol -> ( Random.Seed, String )
render2 seed_ symbol =
    case symbol of
        GTerminal s ->
            case s of
                "_name" ->
                    randomElement seed_ "strong " [ "italic ", "red ", "blue ", "green " ]

                "_x" ->
                    randomElement seed_ "mathematics " [ "physics ", "chemistry ", "biology ", "mountain ", "cloud ", "French ", "German " ]

                _ ->
                    ( seed_, s )

        GTerminalList [] ->
            ( seed_, "1, 2, 3" )

        _ ->
            ( seed_, "" )


randomElement : Random.Seed -> a -> List a -> ( Random.Seed, a )
randomElement seed_ default list =
    let
        ( k, newSeed_ ) =
            Random.step (Random.int 0 (List.length list - 1)) seed_

        element =
            List.Extra.getAt k list |> Maybe.withDefault default
    in
    ( newSeed_, element )


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
    GTerminal "|"


leftBracket =
    GTerminal "["


rightBracket =
    GTerminal "]"
