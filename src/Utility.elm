module Utility exposing (keyValueDict)

import Dict exposing (Dict)
import Maybe.Extra


keyValueDict : List String -> Dict String String
keyValueDict strings =
    List.map (String.split ":") strings
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
