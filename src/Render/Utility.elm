module Render.Utility exposing
    ( getPrecisionWithDefault
    , htmlAttribute
    , makePair
    )

import Dict
import Element exposing (Element)
import Html.Attributes
import Utility


htmlAttribute : String -> String -> Element.Attribute msg
htmlAttribute key value =
    Element.htmlAttribute (Html.Attributes.attribute key value)


getPrecisionWithDefault : Int -> List String -> Int
getPrecisionWithDefault default args =
    getPrecision args |> Maybe.withDefault default


getPrecision : List String -> Maybe Int
getPrecision args =
    let
        dict =
            Utility.keyValueDict args
    in
    Dict.get "precision" dict |> Maybe.andThen String.toInt


makePair : List Float -> Maybe ( Float, Float )
makePair ns =
    case ns of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing
