module Parser.XStringSimple exposing (reduce, text, text_)

{-| Grammar:

    EscapeChar -> '|' | ''[' | ']'
    GoodChar -> any unicode character other than '|', ''[', ']'
    Text -> (GoodChar | EscapeChar)+

    NOT YET IMPLEMENTED:

    RawString -> as in Rust?
    String -> (Text|RawString)+

-}

import Parser exposing ((|.), (|=), Parser)
import Parser.ToolSimple as T


type alias StringData =
    { content : String, start : Int, finish : Int }


text : Parser StringData
text =
    text_
        |> Parser.map reduce


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


text_ : Parser (List StringData)
text_ =
    T.many (Parser.oneOf [ textWithoutEscape, escapedChar ])



-- PREDICATES


isLanguageChar : Char -> Bool
isLanguageChar c =
    c == '|' || c == '[' || c == ']' || c == '\\'


isNonLanguageChar : Char -> Bool
isNonLanguageChar c =
    not (isLanguageChar c)



-- SUBPARSERS


textWithoutEscape : Parser StringData
textWithoutEscape =
    T.text isNonLanguageChar isNonLanguageChar


languageChar : Parser StringData
languageChar =
    T.char isLanguageChar


escapedChar : Parser StringData
escapedChar =
    T.second (Parser.symbol "\\") languageChar
        |> Parser.map (\result -> { content = "\\" ++ result.content, start = result.start - 1, finish = result.finish })
