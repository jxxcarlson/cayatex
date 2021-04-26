module CYUtility exposing (entities, keyValueDict, liftToMaybe, roundTo)

import Dict exposing (Dict)
import Maybe.Extra


roundTo : Int -> Float -> Float
roundTo k x =
    let
        factor =
            10.0 ^ toFloat k
    in
    toFloat (round (factor * x)) / factor


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


liftToMaybe : (a -> b) -> (Maybe a -> Maybe b)
liftToMaybe f =
    \a ->
        case a of
            Nothing ->
                Nothing

            Just a_ ->
                Just (f a_)
