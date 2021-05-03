module Render.Utility exposing
    ( captionElement
    , columnWidths
    , extractText
    , getArg
    , getArgWithDefault
    , getCSV
    , getColumn
    , getInt
    , getPoints
    , getPrecisionWithDefault
    , getTextList
    , htmlAttribute
    , makePair
    , slug
    )

import CYUtility
import Dict exposing (Dict)
import Element as E
import Element.Font as Font
import Html.Attributes
import List.Extra
import Maybe.Extra
import Parser.Element exposing (CYTMsg, Element(..))


slug : String -> String
slug str =
    str |> String.replace " " "-" |> String.toLower


htmlAttribute : String -> String -> E.Attribute msg
htmlAttribute key value =
    E.htmlAttribute (Html.Attributes.attribute key value)


getTextList : Element -> List String
getTextList element =
    case element of
        LX list_ _ ->
            List.map extractText list_
                |> List.map (Maybe.withDefault "")
                |> List.map (String.split ",")
                |> List.map (List.map String.trim)
                |> List.concat

        --  |> Maybe.Extra.values
        _ ->
            []



-- DATA


makePair : List Float -> Maybe ( Float, Float )
makePair ns =
    case ns of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


getPoints : Dict String String -> Element -> List ( Float, Float )
getPoints dict body =
    let
        toInt_ : Int -> String -> Int
        toInt_ default str =
            String.toInt str |> Maybe.withDefault default

        ( col1, col2 ) =
            case ( Dict.get "x-axis" dict, Dict.get "y-axis" dict ) of
                ( Just i, Just j ) ->
                    ( toInt_ 0 i - 1, toInt_ 1 j - 1 )

                _ ->
                    ( 0, 1 )

        xcutoff =
            Dict.get "xcutoff" dict |> Maybe.andThen String.toFloat

        ycutoff =
            Dict.get "ycutoff" dict |> Maybe.andThen String.toFloat

        rawData : List (List String)
        rawData =
            getCSV body

        getDataColumns : Int -> Int -> List (List String) -> List (List (Maybe String))
        getDataColumns i j data =
            List.map (\column -> [ List.Extra.getAt i column, List.Extra.getAt j column ]) rawData

        xfilter points_ =
            case xcutoff of
                Just xcutoffValue ->
                    List.filter (\( x, y ) -> x < xcutoffValue) points_

                _ ->
                    points_

        yfilter points_ =
            case ycutoff of
                Just ycutoffValue ->
                    List.filter (\( x, y ) -> y < ycutoffValue) points_

                _ ->
                    points_
    in
    body
        |> getCSV
        |> getDataColumns col1 col2
        |> List.map Maybe.Extra.values
        |> List.map (List.map String.toFloat)
        |> List.map Maybe.Extra.values
        |> List.map makePair
        |> Maybe.Extra.values
        |> xfilter
        |> yfilter


getCSV : Element -> List (List String)
getCSV element =
    case element of
        LX list_ _ ->
            case List.map extractText list_ of
                [ Just data ] ->
                    data
                        |> String.split "\n"
                        |> List.map (String.split ",")
                        |> List.map (List.map String.trim)

                _ ->
                    [ [] ]

        _ ->
            [ [] ]


itemWidths : List (List String) -> List (List Int)
itemWidths items =
    List.map (List.map String.length) items


columnWidths_ : List (List String) -> List Int
columnWidths_ items =
    items
        |> itemWidths
        |> List.Extra.transpose
        |> List.map List.maximum
        |> Maybe.Extra.values


columnWidths : Float -> Float -> List (List String) -> List Float
columnWidths factor term items =
    items |> columnWidths_ |> List.map (\k -> factor * toFloat k + term)


getColumn : Dict String String -> Element -> List Float
getColumn dict body =
    let
        toInt_ : Int -> String -> Int
        toInt_ default str =
            String.toInt str |> Maybe.withDefault default

        col =
            case Dict.get "column" dict of
                Just i ->
                    toInt_ 0 i - 1

                _ ->
                    0

        cutoff =
            Dict.get "cutoff" dict |> Maybe.andThen String.toFloat

        rawData : List (List String)
        rawData =
            getCSV body

        getDataColumn : Int -> List (List String) -> List (Maybe String)
        getDataColumn i data =
            List.map (\column -> List.Extra.getAt i column) rawData

        filter data_ =
            case cutoff of
                Just cutoffValue ->
                    List.filter (\x -> x < cutoffValue) data_

                _ ->
                    data_
    in
    body
        |> getCSV
        |> getDataColumn col
        |> Maybe.Extra.values
        |> List.map String.toFloat
        |> Maybe.Extra.values
        |> filter



-- ELEMENTS


captionElement dict =
    case Dict.get "caption" dict of
        Just caption ->
            E.paragraph [ Font.bold ] [ E.text caption ]

        Nothing ->
            E.none



-- GETTERS


getPrecisionWithDefault : Int -> List String -> Int
getPrecisionWithDefault default args =
    getPrecision args |> Maybe.withDefault default


getPrecision : List String -> Maybe Int
getPrecision args =
    let
        dict =
            CYUtility.keyValueDict args
    in
    Dict.get "precision" dict |> Maybe.andThen String.toInt


extractText : Element -> Maybe String
extractText element =
    case element of
        Text content _ ->
            Just content

        _ ->
            Nothing


getInt : Int -> List String -> Int
getInt k stringList =
    List.Extra.getAt k stringList
        |> Maybe.map String.trim
        |> Maybe.andThen String.toInt
        |> Maybe.withDefault 0


getArg : Int -> List String -> Maybe String
getArg k stringList =
    List.Extra.getAt k stringList


getArgWithDefault : Int -> String -> List String -> String
getArgWithDefault k default stringList =
    List.Extra.getAt k stringList |> Maybe.withDefault default
