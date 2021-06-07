module Parser.RunLoopFunctions exposing (..)

import Parser.Element exposing (Element)
import Parser.Getters
import Parser.Lines exposing (runLoop, toBlocks, toParsed)



-- ACCESSORS AND TESTERS


rl : String -> List (List Element)
rl str =
    runLoop 0 (String.lines str) |> toParsed |> List.map (List.map Parser.Getters.strip)


{-| Return the block decomposition of a string:

    > bl "abc\n\n[x\n\nQ\n\n[k Q]]\n\ndef"
    ["abc\n","[x\n\nQ\n\n[k Q]]\n\n","def"]

    Anomaly:
    > bl "[x\n\nQ\n\n]"
    ["[x\n\nQ\n\n]","[x\n\nQ\n\n]"]

    Anomaly:
    > bl "[x\n\nQ\n\nQ]"
    ["[x\n\nQ\n\n\nQ]"]

-}
bl : String -> List String
bl str =
    runLoop 0 (String.lines str) |> toBlocks


{-|

    > bbl "abc\n\n[x\n\nQ\n\n[k Q]]\n\ndef"
    ["abc\n","[x\n\nQ\n\n[k Q]]\n\n","def"]

-}
bbl str =
    bl (bl str |> String.join "\n")


{-| For all well-formed strings, bl str == str only modulo some newlines

    > test "abc\n\n[x\n\nQ\n\n[k Q]]\n\ndef"
    False

-}
test str =
    (bl str |> String.join "\n") == str


{-| Idempotency: for all well-formed strings, bbl st

    > test2 "abc\n\n[x\n\nQ\n\n[k Q]]\n\ndef"
    True

-}
test2 str =
    bbl str == bl str


toc str =
    runLoop 0 (String.lines str)
        |> (.data >> .tableOfContents)


rl_ str =
    runLoop 0 (String.lines str) |> toParsed


cl str =
    runLoop 0 (String.lines str)
        |> .output
        |> List.head
        |> Maybe.map (.data >> .counters)
