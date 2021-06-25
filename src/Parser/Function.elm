module Parser.Function exposing
    ( filter
    , getBody
    , getElementTexts
    , getText
    , getTitle
    , isNamed
    , lx
    , setLabel
    )

import Maybe.Extra
import Parser.Element exposing (Element(..), parse)


lx : List Element -> Element
lx list =
    LX list Nothing


setLabel : String -> Element -> Element
setLabel label element_ =
    case element_ of
        Element name args body (Just metadata) ->
            Element name args body (Just { metadata | label = label })

        _ ->
            element_



-- HELPERS
-- GETTERS


{-| Used in module CaYaTeX
-}
getTitle : String -> String
getTitle str =
    case str |> String.trim |> parse 0 0 of
        Err _ ->
            "Untitled"

        Ok elt ->
            case elt of
                Element "title" [] (LX [ Text str_ _ ] _) _ ->
                    str_

                _ ->
                    "Untitled"


getElementTexts : String -> List (List Element) -> List String
getElementTexts elementName_ parsed =
    parsed
        |> filter (isNamed elementName_)
        |> List.map getBody
        |> List.map (Maybe.andThen getText)
        |> Maybe.Extra.values


filter : (Element -> Bool) -> List (List Element) -> List Element
filter predicate list =
    List.map (filter1 predicate) list |> List.concat


filter1 : (Element -> Bool) -> List Element -> List Element
filter1 predicate list =
    List.filter predicate list


isNamed : String -> Element -> Bool
isNamed name element =
    case element of
        Element name_ _ _ _ ->
            name == name_

        _ ->
            False


getBody : Element -> Maybe Element
getBody element =
    case element of
        Element _ _ body _ ->
            Just body

        Text _ _ ->
            Nothing

        LX _ _ ->
            Nothing

        Problem _ _ _ ->
            Nothing


getText : Element -> Maybe String
getText element =
    case element of
        LX [ Text str _ ] _ ->
            Just str

        _ ->
            Nothing
