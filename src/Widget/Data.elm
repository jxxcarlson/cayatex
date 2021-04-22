module Widget.Data exposing (bargraph, linegraph, scatterplot)

import Element as E exposing (column, el, paragraph, px, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Maybe.Extra
import Parser.Element exposing (Element(..), Mark2Msg)
import Render.Types exposing (FRender)
import Render.Utility
import SimpleGraph exposing (Option(..), barChart, lineChart, scatterPlot)
import Utility



-- DATA


bargraph : FRender Mark2Msg
bargraph renderArgs name args body sm =
    let
        dict =
            Utility.keyValueDict args

        numbers : List Float
        numbers =
            Render.Utility.getColumn dict body
                |> List.map (\x -> x + 0.5)

        dataMax =
            List.maximum numbers |> Maybe.withDefault 0

        dataMin =
            List.minimum numbers |> Maybe.withDefault 0

        n =
            List.length numbers |> toFloat

        graphHeight =
            200.0

        graphWidth =
            300.0

        deltaX =
            graphWidth / n

        options =
            [ Color "rgb(200,0,0)", DeltaX deltaX, YTickmarks 6, XTickmarks (round (n + 1)), Scale 1.0 1.0 ]

        barGraphAttributes =
            { graphHeight = graphHeight
            , graphWidth = graphWidth
            , options = options
            }
    in
    column []
        [ barChart barGraphAttributes (List.map (\x -> x + 0.001) numbers) |> E.html
        , Render.Utility.captionElement dict
        , paragraph [ spacing 12 ]
            [ text ("data points: " ++ String.fromFloat n ++ ", ")
            , text ("min: " ++ String.fromFloat (Utility.roundTo 2 dataMin) ++ ", ")
            , text ("max: " ++ String.fromFloat (Utility.roundTo 2 dataMax))
            ]
        ]


linegraph : FRender Mark2Msg
linegraph renderArgs name args body sm =
    let
        dict =
            Utility.keyValueDict args

        numbers_ : List (List String)
        numbers_ =
            Render.Utility.getCSV body

        points : List ( Float, Float )
        points =
            List.map (List.map String.toFloat) numbers_
                |> List.map Maybe.Extra.values
                |> List.map Render.Utility.makePair
                |> Maybe.Extra.values

        n =
            List.length points |> toFloat

        graphHeight =
            200.0

        graphWidth =
            350.0

        deltaX =
            graphWidth / n

        options =
            [ Color "rgb(0,0,200)", DeltaX deltaX, YTickmarks 6, XTickmarks (round (n + 1)), Scale 1.0 1.0 ]

        lineGraphAttributes =
            { graphHeight = graphHeight
            , graphWidth = graphWidth
            , options = options
            }
    in
    column []
        [ lineChart lineGraphAttributes points |> E.html
        , Render.Utility.captionElement dict
        ]


scatterplot : FRender Mark2Msg
scatterplot renderArgs name args body sm =
    let
        dict =
            Utility.keyValueDict args

        points =
            Render.Utility.getPoints dict body

        xmax =
            List.maximum (List.map Tuple.first points) |> Maybe.withDefault 0

        ymax =
            List.maximum (List.map Tuple.second points) |> Maybe.withDefault 0

        n =
            List.length points |> toFloat

        graphHeight =
            400.0

        graphWidth =
            400.0

        deltaX =
            graphWidth / n

        options =
            [ Color "rgb(0,0,200)", DeltaX deltaX, YTickmarks 6, XTickmarks (round (n + 1)), Scale 1.0 1.0 ]

        scatterPlotAttributes =
            { graphHeight = graphHeight
            , graphWidth = graphWidth
            , options = options
            }

        points2 =
            List.map (\( x, y ) -> ( x - 0.03, y )) points
    in
    column []
        [ scatterPlot scatterPlotAttributes points2 |> E.html
        , Render.Utility.captionElement dict
        , paragraph [ spacing 12 ]
            [ text ("data points: " ++ String.fromFloat n ++ ", ")
            , text ("xmax: " ++ String.fromFloat (Utility.roundTo 0 xmax) ++ ", ")
            , text ("ymax: " ++ String.fromFloat (Utility.roundTo 0 ymax))
            ]
        ]
