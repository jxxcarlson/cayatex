module Parser.XString exposing (isNonLanguageChar, isNotExtendedLanguageChar, textListWithPredicate, textWithPredicate)

{-| Grammar:

    EscapeChar -> '|' | ''[' | ']'
    GoodChar -> any unicode character other than '|', ''[', ']'
    Text -> (GoodChar | EscapeChar)+

    NOT YET IMPLEMENTED:

    RawString -> as in Rust?
    String -> (Text|RawString)+

-}

{- (text -}

import Parser.Advanced exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Tool as T


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type alias StringData =
    { content : String, start : Int, finish : Int }


textWithPredicate : (Char -> Bool) -> Parser StringData
textWithPredicate predicate =
    textListWithPredicate predicate
        |> Parser.Advanced.map reduce


reduce : List StringData -> StringData
reduce list =
    let
        start =
            list |> List.head |> Maybe.map .start |> Maybe.withDefault 0

        reversedList =
            List.reverse list

        finish =
            reversedList |> List.head |> Maybe.map .finish |> Maybe.withDefault 0
    in
    { content = List.foldl (++) "" (List.map .content reversedList), start = start, finish = finish }


textListWithPredicate : (Char -> Bool) -> Parser (List StringData)
textListWithPredicate predicate =
    T.manyNonEmpty (Parser.Advanced.oneOf [ textWithPredicate_ predicate, escapedChar ])



-- PREDICATES


isLanguageChar : Char -> Bool
isLanguageChar c =
    c == '|' || c == '[' || c == ']' || c == '\\'


isExtendedLanguageChar : Char -> Bool
isExtendedLanguageChar c =
    c == '|' || c == '[' || c == ']' || c == ',' || c == '\\'


isNonLanguageChar : Char -> Bool
isNonLanguageChar c =
    not (isLanguageChar c)


isNotExtendedLanguageChar : Char -> Bool
isNotExtendedLanguageChar c =
    not (isExtendedLanguageChar c)



-- SUBPARSERS


textWithPredicate_ : (Char -> Bool) -> Parser StringData
textWithPredicate_ predicate =
    T.text predicate (\c -> c /= '\\' && predicate c)


textWithoutEscape : Parser StringData
textWithoutEscape =
    T.text isNonLanguageChar isNonLanguageChar


languageChar : Parser StringData
languageChar =
    T.char isLanguageChar


escapedChar : Parser StringData
escapedChar =
    T.second (Parser.Advanced.symbol (Parser.Advanced.Token "\\" ExpectingEscape)) (T.char (\c -> True))
        |> Parser.Advanced.map (\result -> { content = "\\" ++ result.content, start = result.start - 1, finish = result.finish })
