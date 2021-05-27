module Parser.Line exposing (classify)

import Parser as P exposing ((|.), (|=))
import Parser.Types exposing (LineType(..))



-- CLASSIFY LINE


classify : String -> LineType
classify str =
    case P.run lineTypeParser str of
        Ok lt ->
            lt

        Err _ ->
            LTBlank


lineTypeParser =
    P.oneOf [ commentParser, beginElementParser, endElementParser, textBlockParser, P.succeed LTBlank ]


beginElementParser : P.Parser LineType
beginElementParser =
    P.succeed (\s -> LTBeginElement)
        |= P.symbol "["


commentParser : P.Parser LineType
commentParser =
    P.succeed (\s -> LTComment)
        |= P.symbol "%"



--|= P.getChompedString (P.chompUntil "[")


endElementParser : P.Parser LineType
endElementParser =
    P.succeed (\s -> LTEndElement)
        |= P.symbol "]"


textBlockParser : P.Parser LineType
textBlockParser =
    P.succeed LTTextBlock
        |. P.chompIf (\_ -> True)
