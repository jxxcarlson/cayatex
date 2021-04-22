module Widget.Conway exposing (State(..), new, occupied, occupy, spot, step, toggleStateAt)

import Array
import CellGrid exposing (CellGrid(..), CellType(..), Dimensions, Position)
import Random


type State
    = Occupied
    | Unoccupied


toggle : State -> State
toggle state =
    case state of
        Occupied ->
            Unoccupied

        Unoccupied ->
            Occupied


step : CellGrid State -> CellGrid State
step cellGrid =
    CellGrid.transform (\i j -> nextValueAt (Position i j)) cellGrid


new : Int -> Float -> Dimensions -> CellGrid State
new seed density dimensions =
    Random.step (generator dimensions { density = density }) (Random.initialSeed seed)
        |> Tuple.first


occupy : Position -> CellGrid State -> CellGrid State
occupy position cg =
    CellGrid.set position Occupied cg


toggleStateAt : Position -> CellGrid State -> CellGrid State
toggleStateAt position cellGrid =
    CellGrid.update position toggle cellGrid


spot : ( Int, Int ) -> Float -> State -> CellGrid State -> CellGrid State
spot ( centerI, centerJ ) radius state cg =
    let
        cellTransformer : Int -> Int -> State -> State
        cellTransformer i j t =
            let
                di =
                    toFloat <| i - centerI

                dj =
                    toFloat <| j - centerJ
            in
            if di * di + dj * dj <= radius * radius then
                state

            else
                t
    in
    CellGrid.indexedMap cellTransformer cg


nextValueAt : Position -> CellGrid State -> State
nextValueAt position cellGrid =
    case CellGrid.get position cellGrid of
        Nothing ->
            Unoccupied

        Just state ->
            let
                nOccupied =
                    occupiedNeighbors position cellGrid
            in
            case ( state, nOccupied ) of
                ( Unoccupied, 3 ) ->
                    Occupied

                ( Unoccupied, _ ) ->
                    Unoccupied

                ( Occupied, 2 ) ->
                    Occupied

                ( Occupied, 3 ) ->
                    Occupied

                ( Occupied, _ ) ->
                    Unoccupied


occupied : CellGrid State -> Int
occupied grid =
    let
        folder state count =
            if state == Occupied then
                count + 1

            else
                count
    in
    CellGrid.foldl folder 0 grid


occupiedNeighbors : Position -> CellGrid State -> Int
occupiedNeighbors position grid =
    let
        folder state count =
            if state == Occupied then
                count + 1

            else
                count
    in
    CellGrid.neighbors position grid
        |> List.foldl folder 0



-- RANDOM GENERATORS


{-| Generate a cell value. Density is a number between 0 and 1, which we interpret as a percentage.

We want `density` percent of the cells to be occupied, thus `1 - density` percent of cells to be unoccupied.
We use a weighted random distribution, that will give approximately the desired number of occupied cells.

-}
generateCell : { density : Float } -> Random.Generator State
generateCell { density } =
    Random.weighted
        ( density, Occupied )
        [ ( 1 - density, Unoccupied )
        ]


{-| Generate a cell grid
-}
generator : Dimensions -> { density : Float } -> Random.Generator (CellGrid State)
generator dimensions config =
    Random.list (dimensions.rows * dimensions.columns) (generateCell config)
        |> Random.map (Array.fromList >> CellGrid dimensions)
