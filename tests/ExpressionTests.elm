module ExpressionTests exposing (..)


import Parser.Expression exposing (..)
import Expect
import Test exposing (describe, fuzz, test)
import Parser.Advanced exposing(run)


-- differTestFunction : String -> String -> String -> DiffRecord String -> Test.Test
-- differTestFunction desc text1 text2 expectedDiffRecord =
--     let
--         blocks1 : List String
--         blocks1 =
--             Block.compile 0 (String.lines text1)
--                 |> List.map (String.join "\n")

--         blocks2 : List String
--         blocks2 =
--             Block.compile 0 (String.lines text2)
--                 |> List.map (String.join "\n")

--         blockDiffRecord =
--             diff blocks1 blocks2
--     in
--     test desc <|
--         \_ -> Expect.equal blockDiffRecord expectedDiffRecord


suite0 =
    describe "Nothing"
        [ test "zero" <| \_ -> Expect.equal 2 2
        ]


suite =
    describe "Parser"
        [ describe "exoresssion"
            [ test "Text" <| 
              \_ -> Expect.equal 
                (run (text_ 0 0 []) "this is a test")
                (Ok (Text ("this is a test") (Just { blockOffset = 0, content = "this is a test", generation = 0, length = 14, offset = 0 })))
              , test "Image" <| 
                \_ -> Expect.equal
                (run (inlineExpression 1 2) "[image [height:40,width:100] stuff]" |> Result.map strip)
                (Ok (Inline "image" ["height:40" ,"width:100"] ("stuff") Nothing))

            ]
        ]   



 