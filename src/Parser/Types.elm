module Parser.Types exposing (LineType(..))

{-| -}


type LineType
    = LTBlank
    | LTTextBlock
    | LTBeginElement
    | LTEndElement
    | LTComment
