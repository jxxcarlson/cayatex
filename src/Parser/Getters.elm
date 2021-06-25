module Parser.Getters exposing (..)

import Parser.Element exposing (Element(..))
import Parser.Metadata exposing (Metadata)


{-| Set the SourceMap to Nothing
-}
strip : Element -> Element
strip expr =
    case expr of
        Text str _ ->
            Text str Nothing

        Element name args body_ _ ->
            Element name args (strip body_) Nothing

        LX expr_ _ ->
            LX (List.map strip expr_) Nothing

        Problem p e _->
            Problem p e Nothing


getSource : Element -> Maybe Metadata
getSource expr =
    case expr of
        Text _ sm ->
            sm

        Element _ _ _ sm ->
            sm

        LX expr_ sm ->
            sm

        Problem _ _ _ ->
            Nothing



-- getArgs : Expression -> Maybe Expression


getArgs_ expr =
    case expr of
        Text _ _ ->
            Nothing

        Element _ args_ _ _ ->
            Nothing

        LX expr_ _ ->
            Nothing

        Problem _ _ _ ->
            Nothing


getArgs expr =
    expr |> List.map getArgs_ |> List.map (Maybe.map (List.map strip))


getBody_ expr =
    case expr of
        Text str _ ->
            Nothing

        Element _ _ body_ _ ->
            Nothing

        LX _ _ ->
            Nothing

        Problem _ _ _ ->
            Nothing


getBody =
    List.map (getBody_ >> Maybe.map strip)
