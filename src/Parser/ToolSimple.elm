module Parser.ToolSimple exposing
    ( Step(..)
    , between
    , char
    , first
    , loop
    , many
    , manyNonEmpty
    , manySeparatedBy
    , maybe
    , optional
    , optionalList
    , second
    , text
    )

import Parser exposing ((|.), (|=), Parser)


{-| Apply a parser zero or more times and return a list of the results.

Example:

    > bracketedInt = first (between (Parser.symbol "[") Parser.int (Parser.symbol "]")) Parser.spaces

    > run (many bracketedInt) ""
    Ok []

    > run (many bracketedInt) "[1] [2] [3]"
    Ok [1,2,3]rn

-}
many : Parser a -> Parser (List a)
many p =
    Parser.loop [] (manyHelp p)


{-| Return a list of things recognized by parser p
that are separated by things recognized by parser sep.
-}
manySeparatedBy : Parser () -> Parser a -> Parser (List a)
manySeparatedBy sep p =
    manyNonEmpty_ p (second sep p)


manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp p vs =
    Parser.oneOf
        [ Parser.end |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        , Parser.succeed (\v -> Parser.Loop (v :: vs))
            |= p
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        ]


{-| Return a nonempty list of things recognized by parser p. Fail
if p does not recognize anything.

    > run (manyNonEmpty bracketedInt) ""
    Err [{ col = 1, problem = ExpectingSymbol "[", row = 1 }]

    > run (manyNonEmpty bracketedInt) "[1]"
    Ok [1]

-}
manyNonEmpty : Parser a -> Parser (List a)
manyNonEmpty p =
    p
        |> Parser.andThen (\x -> manyWithInitialList [ x ] p)


manyNonEmpty_ : Parser a -> Parser a -> Parser (List a)
manyNonEmpty_ p q =
    p
        |> Parser.andThen (\x -> manyWithInitialList [ x ] q)


manyWithInitialList : List a -> Parser a -> Parser (List a)
manyWithInitialList initialList p =
    Parser.loop initialList (manyHelp p)


{-| Running `optional p` means run p, but if it fails, succeed anyway

    > run (optional (Parser.symbol "@")) "@"
    Ok ()

    > run (optional (Parser.symbol "@")) "!"
    Ok ()

-}
optional : Parser () -> Parser ()
optional p =
    Parser.oneOf [ p, Parser.succeed () ]


{-| Running `optional p` means run p. If the parser succeeds with value _result_,
return _Just result_ . If the parser failes, return Nothing.

    > run (maybe Parser.int) "3"
    Ok (Just 3)

    > run (maybe Parser.int) "three"
    Ok Nothing

-}
maybe : Parser a -> Parser (Maybe a)
maybe p =
    Parser.oneOf [ p |> Parser.map (\x -> Just x), Parser.succeed () |> Parser.map (\_ -> Nothing) ]


{-| Running `optionalList p` means run p, but if it fails, succeed anyway,
returning the empty list

    > run (optionalList (many bracketedInt)) "three"
    > Ok []

    > run (optionalList (many bracketedInt)) "[3] [4]"
    Ok [3,4]

-}
optionalList : Parser (List a) -> Parser (List a)
optionalList p =
    Parser.oneOf [ p, Parser.succeed () |> Parser.map (\_ -> []) ]


{-| running `first p q` means run p, then run q
and return the result of running p.
-}
first : Parser a -> Parser b -> Parser a
first p q =
    p |> Parser.andThen (\x -> q |> Parser.map (\_ -> x))


{-| running `second p q` means run p, then run q
and return the result of running q.
-}
second : Parser a -> Parser b -> Parser b
second p q =
    p |> Parser.andThen (\_ -> q)


{-| Running between p q r runs p, then q, then r, returning the result of p:

> run (between (Parser.symbol "[") Parser.int (Parser.symbol "]")) "[12]"
> Ok 12

-}
between : Parser a -> Parser b -> Parser c -> Parser b
between p q r =
    first (second p q) r


{-| text prefixText suffixText": Get the longest string
whose first character satisfies the prefixTest and whose remaining
characters satisfy the bodyTest. Example:

    line =
        text (\c -> Char.isAlpha) (\c -> c /= '\n')

recognizes lines that start with an alphabetic character.

-}
text : (Char -> Bool) -> (Char -> Bool) -> Parser { start : Int, finish : Int, content : String }
text prefixTest bodyTest =
    Parser.succeed (\start finish content -> { start = start, finish = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefixTest c)
        |. Parser.chompWhile (\c -> bodyTest c)
        |= Parser.getOffset
        |= Parser.getSource


char : (Char -> Bool) -> Parser { start : Int, finish : Int, content : String }
char prefixTest =
    Parser.succeed (\start finish content -> { start = start, finish = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefixTest c)
        |= Parser.getOffset
        |= Parser.getSource



-- LOOP


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
