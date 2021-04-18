module Utility exposing (entities, keyValueDict)

import Dict exposing (Dict)
import Maybe.Extra


entities : List String -> List String
entities strings_ =
    List.map (String.split ":") strings_
        |> List.map (List.map String.trim)
        |> List.filter (\x -> List.length x == 1)
        |> List.map List.head
        |> Maybe.Extra.values


keyValueDict : List String -> Dict String String
keyValueDict strings_ =
    List.map (String.split ":") strings_
        |> List.map (List.map String.trim)
        |> List.map pairFromList
        |> Maybe.Extra.values
        |> Dict.fromList


pairFromList : List String -> Maybe ( String, String )
pairFromList strings =
    case strings of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing
