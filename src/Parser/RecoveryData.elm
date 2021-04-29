module Parser.RecoveryData exposing (..)

import Parser.Element as Element exposing (Element(..))
import Parser.Error exposing (Problem(..))
import Parser.Metadata as SourceMap exposing (Metadata)
import Parser.TextCursor exposing (TextCursor)


{-| -}
type alias RecoveryData =
    { problem : Problem
    , deltaOffset : Int
    , textTruncation : Int
    , parseSubstitute : Element
    }


{-| -}
get : TextCursor Element -> Problem -> Maybe RecoveryData
get tc_ problem =
    let
        oldSourceMap =
            SourceMap.dummy

        newSourceMap =
            Just { oldSourceMap | blockOffset = tc_.blockIndex }
    in
    get_ problem
        |> Maybe.map (\r -> { r | parseSubstitute = setSourceMap newSourceMap r.parseSubstitute })


get_ : Problem -> Maybe RecoveryData
get_ problem =
    List.filter (\r -> r.problem == problem) recoveryData |> List.head


recoveryData : List RecoveryData
recoveryData =
    [ problemWithElement ]


problemWithElement : RecoveryData
problemWithElement =
    { problem = ExpectingRightBracket
    , deltaOffset = 1
    , textTruncation = 1
    , parseSubstitute =
        Element "fontRGB"
            [ "255", "0", "0" ]
            (Text (String.fromChar '⚠' ++ " unmatched $ in" ++ String.fromChar '\u{00A0}') Nothing)
            Nothing
    }



-- ERRORS


{-| getErrors takes parsed text as input and produces a list of errors as output.
-}
getErrors : List (List Element) -> List Element
getErrors list =
    list
        |> List.map getErrors_
        |> List.concat


getErrors_ : List Element -> List Element
getErrors_ list =
    List.filter (\e -> isError e) list


isError : Element -> Bool
isError expr =
    case expr of
        Element name _ _ _ ->
            name == "Error"

        _ ->
            False



-- HELPERS


{-| -}
setSourceMap : Maybe Metadata -> Element -> Element
setSourceMap sm expr =
    case expr of
        Text e _ ->
            Text e sm

        Element name args body _ ->
            Element name args body sm

        LX list _ ->
            LX list sm



--type Element
--    = Text String (Maybe SourceMap)
--    | Element String (List String) Element (Maybe SourceMap)
--    | LX (List Element) (Maybe SourceMap)
