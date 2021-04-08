module Parser.XString exposing (text)

{-| Grammar:

    EscapeChar -> '|' | ''[' | ']'
    GoodChar -> any unicode character other than '|', ''[', ']'
    Text -> (GoodChar | EscapeChar)+

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
    T.many (Parser.oneOf [ goodTextWithoutEscape, escapedChar ])



-- HELPERS and SUBPARSERS


isNonLanguageChar : Char -> Bool
isNonLanguageChar =
    \c -> c /= '|' && c /= '[' && c /= ']' && c /= '\\'


isLanguageChar : Char -> Bool
isLanguageChar =
    \c -> c /= '|' || c /= '[' || c /= ']' || c /= '\\'


goodTextWithoutEscape : Parser StringData
goodTextWithoutEscape =
    T.text isNonLanguageChar isNonLanguageChar


languageChar : Parser StringData
languageChar =
    T.char isLanguageChar


escapedChar : Parser StringData
escapedChar =
    T.second (Parser.symbol "\\") languageChar
        |> Parser.map (\result -> { content = "\\" ++ result.content, start = result.start, finish = result.finish + 1 })
