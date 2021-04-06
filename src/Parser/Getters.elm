module Parser.Getters exposing (..)

import Parser.Expression exposing(Expression(..))
import Parser.SourceMap exposing(SourceMap)


{-| Set the SourceMap to Nothing
-}
strip : Expression -> Expression
strip expr =
    case expr of
        Text str _ ->
            Text str Nothing

        Inline name args body_ _ ->
            Inline name args body_ Nothing

        Block name args body_ _ ->
            Block name (List.map strip args) (Maybe.map strip body_) Nothing

        List expr_ _ ->
            List expr_ Nothing


getSource : Expression -> Maybe SourceMap
getSource expr =
    case expr of
        Text _ sm ->
            sm

        Inline _ _ _ sm ->
            sm

        Block _ _ _ sm ->
            sm

        List expr_ sm ->
            sm



-- getArgs : Expression -> Maybe Expression


getArgs_ expr =
    case expr of
        Text _ _ ->
            Nothing

        Inline _ args_ _ _ ->
            Nothing

        Block _ args_ _ _ ->
            Just args_

        List expr_ _ ->
            Nothing


getArgs expr =
    expr |> List.map getArgs_ |> List.map (Maybe.map (List.map strip))


getBody_ expr =
    case expr of
        Text str _ ->
            Nothing

        Inline _ _ body_ _ ->
            Nothing

        Block _ _ body_ _ ->
            body_

        List _ _ ->
            Nothing


getBody =
    List.map (getBody_ >> Maybe.map strip)
