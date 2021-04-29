module Render.Types exposing (DisplayMode(..), FRender, RenderArgs, RenderElementDict)

import Dict exposing (Dict)
import Element as E
import Parser.Data
import Parser.Element exposing (CYTMsg, Element(..))
import Parser.Metadata exposing (Metadata)



-- TYPES


type alias RenderArgs =
    { width : Int
    , selectedId : String
    , generation : Int
    , blockOffset : Int
    , parserData : Parser.Data.Data
    }


type alias FRender a =
    RenderArgs -> String -> List String -> Element -> Maybe Metadata -> E.Element a


type alias RenderElementDict a =
    Dict String (FRender a)


type DisplayMode
    = InlineMathMode
    | DisplayMathMode
