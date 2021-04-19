module Render.Utility exposing (..)

import Element exposing (Element)
import Html.Attributes


htmlAttribute : String -> String -> Element.Attribute msg
htmlAttribute key value =
    Element.htmlAttribute (Html.Attributes.attribute key value)
