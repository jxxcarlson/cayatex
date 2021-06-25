module Parser.RawString exposing (parser, r0, r1, r2, r3)

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Tool as T


r0 =
    "raw#abc#"


r1 =
    "raw#[abc]#"


r2 =
    "raw#[3]#"


r3 =
    "raw#[3#"


type alias Parser a =
    Parser.Parser Context Problem a


rawStringPrefix : Parser Int
rawStringPrefix =
    Parser.succeed (\begin end -> end - begin - 3)
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "raw#" ExpectingRawPrefix)
        |. Parser.chompWhile (\c -> c == '#')
        |= Parser.getOffset


type alias LoopState =
    { hashCount : Int, content : String }


type alias NumberOfHashes =
    Int


{-|

    > A.run rawString "raw##a #[b],[m]# c##ho ho ho"
    Ok ("a #[b],[m]# c")

-}
parser : Parser String
parser =
    rawStringPrefix
        |> Parser.andThen (\maxHashes -> rawStringLoop maxHashes |> Parser.map (\ls -> ls.content |> String.dropRight maxHashes))


rawStringLoop : Int -> Parser LoopState
rawStringLoop hashes =
    Parser.loop { hashCount = 0, content = "" } (rawStringHelp hashes)


rawStringHelp : Int -> LoopState -> Parser (Parser.Step LoopState LoopState)
rawStringHelp hashes state =
    if state.hashCount >= hashes then
        Parser.succeed (Parser.Done state)

    else
        T.oneChar |> Parser.map (\c -> updateState hashes c state)


updateState : Int -> String -> LoopState -> Parser.Step LoopState LoopState
updateState maxHashes c state =
    if c == "#" then
        if String.right 1 state.content == "#" then
            if state.hashCount + 1 > state.hashCount + 1 then
                Parser.Done { hashCount = state.hashCount + 1, content = state.content ++ c }

            else
                Parser.Loop { hashCount = state.hashCount + 1, content = state.content ++ c }

        else
            Parser.Loop { hashCount = 1, content = state.content ++ c }

    else
        Parser.Loop { hashCount = 0, content = state.content ++ c }
