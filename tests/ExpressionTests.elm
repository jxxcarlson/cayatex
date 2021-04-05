module ExpressionTests exposing (..)


import Parser.Expression exposing (..)
import Expect
import Test exposing (describe, fuzz, test)
import Parser.Advanced exposing(run)

suite =
    describe "Parser.Expression"
        [ describe "parser"
            [ test "Text" <| 
              \_ -> Expect.equal 
                (run (parser 0 0) "this is a test")
                (Ok (Text ("this is a test") (Just { blockOffset = 0, generation = 0, length = 14, offset = 0 })))
              , test "Inline" <| 
                \_ -> Expect.equal
                (run (parser 1 2) "[image [height:40,width:100] stuff]" |> Result.map strip)
                (Ok (Inline "image" ["height:40" ,"width:100"] ("stuff") Nothing))
              , test "Block" <|
                 \_ -> Expect.equal
                 (run (parser 1 2) "|yada [7, 5] foo bar |end mmm" |> Result.map strip)
                 (Ok (Block "yada" ["7","5"] (Just (Text ("foo bar ") (Just { blockOffset = 2, generation = 1, length = 21, offset = 0 }))) Nothing))
            ]
        ]   



 