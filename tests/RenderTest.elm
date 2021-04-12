module RenderTest exposing (..)

import Expect
import Render.String exposing (renderString)
import Test exposing (describe, fuzz, test)


suite =
    describe "Render.String"
        [ describe "render"
            [ test "text" <|
                \_ ->
                    Expect.equal
                        (renderString "This is a test")
                        "<span>This is a test</span>"
            ]
        ]
