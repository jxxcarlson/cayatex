module RenderTest exposing (..)

import Expect
import Render.String exposing (renderString_)
import Test exposing (describe, fuzz, test)


suite =
    describe "Render.String"
        [ describe "render"
            [ test "text" <|
                \_ ->
                    Expect.equal
                        (renderString_ "This is a test")
                        "<span>This is a test</span>"
            ]
        ]
