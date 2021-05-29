module Parser.Types exposing (..)

import Parser.Data
import Parser.Element exposing (Element(..))
import Parser.TextCursor as TextCursor exposing (TextCursor)


type alias Document =
    { prelude : Lines
    , sections : List Section
    }


type alias Line =
    { index : Int, content : String }


type alias Lines =
    List Line


type alias Section =
    Lines


type LineType
    = LTBlank
    | LTTextBlock
    | LTBeginElement
    | LTEndElement
    | LTComment


{-| -}
type alias State =
    { input : List String
    , lineNumber : Int
    , generation : Int
    , blockStatus : BlockStatus
    , blockContents : List String
    , blockLevel : Int
    , lastTextCursor : Maybe (TextCursor Element)
    , output : List (TextCursor Element)
    , data : Parser.Data.Data
    }


{-| -}
type alias Block =
    { blockType : BlockStatus, content : List String }


{-| -}
type BlockStatus
    = Start
    | InTextBlock
    | InElementBlock
