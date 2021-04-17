module Parser.Document exposing
    ( process, toParsed, toText
    , State, Block, BlockType(..), LineType(..)
    , run
    )

{-| The main function in this module is process, which takes as input
a string representing a MiniLaTeX document and produces as output
a value of type AST = List (List Expression).


## Functions

@docs process, toParsed, toText


## Types

@docs State, Block, BlockType, LineType

##@ About blocks

The block offset is the line number in the source text
at wwhich a block begins. Recall that a block is a string (which may contain newlines) derived
from a set of contiguous lines. It represents a logical paragraph: either an ordinary paragraph or
an outer begin-end pair. The offset describes the position of a string in the block. Thus, if
the text "a\\nb\\nc" starts at line 100 of the source text and is preceded and followed by a blank line,
then the block offset is 100, the offset of "a" is 0, the offest of "b" is 2, and the offest of "c" is 4.

-}

-- import Parser.Parser as Parser
--import Render.LaTeXState as LaTeXState exposing (LaTeXState)
--import Render.Reduce as Reduce

import Parser as P exposing ((|.), (|=))
import Parser.Driver
import Parser.Element exposing (Element)
import Parser.TextCursor as TextCursor exposing (TextCursor)


{-| -}
type alias State =
    { input : List String
    , lineNumber : Int
    , generation : Int
    , blockType : BlockType
    , blockContents : List String
    , blockTypeStack : List BlockType
    , output : List (TextCursor Element)

    --, laTeXState : LaTeXState
    }


{-| -}
type alias Block =
    { blockType : BlockType, content : List String }


{-| -}
type BlockType
    = Start
    | TextBlock
    | ElementBlock
    | ErrorBlock


{-| -}
type LineType
    = LTBlank
    | LTTextBlock
    | BeginElementBlock
    | EndElementBlock


{-| Compute the syntax tree and LaTeXState of a string of source text.
-}
process : Int -> List String -> State
process generation =
    run generation



-- >> toParsed


{-| Compute the final State of a string of source text.
The output field of State holds the AST of the source text.

Function process operates a state machine which identifies logical
chunks of text, parses these using Parser.Parser.parseLoop,
and prepends them to a list of TextCursor.

-}
run : Int -> List String -> State
run generation strList =
    loop (init generation strList) nextState


{-| Return the AST from the State.
-}
toParsed : State -> List (List Element)
toParsed state =
    state.output |> List.map .parsed


{-| Return a list of logical paragraphs (blocks(: ordinary paragraphs
or outer begin-end blocks.
-}
toText : State -> List String
toText state =
    state.output |> List.map .block


init : Int -> List String -> State
init generation strList =
    { input = strList
    , lineNumber = 0
    , generation = generation
    , blockType = Start
    , blockContents = []
    , blockTypeStack = []
    , output = []

    --  , laTeXState = LaTeXState.init
    }


nextState : State -> Step State State
nextState state_ =
    case List.head state_.input of
        Nothing ->
            Done (flush state_)

        Just currentLine ->
            let
                state =
                    { state_ | input = List.drop 1 state_.input }
            in
            case ( state.blockType, classify currentLine ) of
                ( Start, LTBlank ) ->
                    Loop (start state)

                ( Start, BeginElementBlock ) ->
                    Loop (pushBlockStack currentLine state)

                ( Start, EndElementBlock ) ->
                    Loop (initBlock ErrorBlock currentLine state)

                ( Start, LTTextBlock ) ->
                    Loop (initBlock TextBlock currentLine state)

                --
                ( ErrorBlock, LTBlank ) ->
                    Loop { state | blockType = Start, blockContents = [] }

                ( ErrorBlock, BeginElementBlock ) ->
                    Loop (initWithBlockType currentLine state)

                ( ErrorBlock, EndElementBlock ) ->
                    Loop (addToBlockContents currentLine state)

                ( ErrorBlock, LTTextBlock ) ->
                    Loop (initWithBlockType currentLine state)

                --
                ( TextBlock, LTBlank ) ->
                    -- Then end of a text block has been reached. Create a string representing
                    -- this block, parse it using Parser.parseLoop to produce a TextCursor, and
                    -- add it to state.output.  Finally, update the laTeXState using Render.Reduce.latexState
                    Loop (pushBlock state)

                ( TextBlock, BeginElementBlock ) ->
                    Loop (initWithBlockType currentLine state)

                ( TextBlock, EndElementBlock ) ->
                    Loop (addToBlockContents currentLine state)

                ( TextBlock, LTTextBlock ) ->
                    Loop (addToBlockContents currentLine state)

                ---
                ( ElementBlock, LTBlank ) ->
                    Loop state

                ( ElementBlock, BeginElementBlock ) ->
                    Loop (pushBlockStack currentLine state)

                ( ElementBlock, EndElementBlock ) ->
                    Loop (popBlockStack currentLine state)

                ( ElementBlock, LTTextBlock ) ->
                    Loop (addToBlockContents currentLine state)



-- OPERATIONS ON STATE


{-| Put State in the Start state
-}
start : State -> State
start state =
    { state | blockType = Start, blockTypeStack = [], blockContents = [] }


initBlock : BlockType -> String -> State -> State
initBlock blockType_ currentLine_ state =
    { state | blockType = blockType_, blockContents = [ currentLine_ ] }


initWithBlockType : String -> State -> State
initWithBlockType currentLine_ state =
    let
        newTC =
            Parser.Driver.parseLoop state.generation state.lineNumber (String.join "\n" (List.reverse (currentLine_ :: state.blockContents)))

        --laTeXState =
        --    Reduce.laTeXState newTC.parsed state.laTeXState
    in
    { state
        | blockContents = [ currentLine_ ]
        , lineNumber = state.lineNumber + countLines state.blockContents

        --, laTeXState = laTeXState
        , output = newTC :: state.output
    }


addToBlockContents : String -> State -> State
addToBlockContents currentLine_ state =
    { state | blockContents = currentLine_ :: state.blockContents }


pushBlockStack : String -> State -> State
pushBlockStack currentLine_ state =
    { state
        | blockContents = currentLine_ :: state.blockContents
    }


pushBlock : State -> State
pushBlock state =
    let
        tc : TextCursor Element
        tc =
            Parser.Driver.parseLoop state.generation state.lineNumber (String.join "\n" (List.reverse state.blockContents))
    in
    { state
        | blockType = Start
        , blockContents = []

        -- , laTeXState = Reduce.laTeXState tc.parsed state.laTeXState
        , output = tc :: state.output
        , lineNumber = state.lineNumber + countLines state.blockContents
    }


popBlockStack : String -> State -> State
popBlockStack currentLine_ state =
    let
        newBlockTypeStack =
            List.drop 1 state.blockTypeStack
    in
    if newBlockTypeStack == [] then
        let
            input_ =
                String.join "\n" (List.reverse (currentLine_ :: state.blockContents))

            tc_ =
                Parser.Driver.parseLoop state.generation state.lineNumber input_

            tc =
                { tc_ | text = input_ }
        in
        { state
            | blockType = Start
            , blockTypeStack = []
            , blockContents = currentLine_ :: state.blockContents
            , output = tc :: state.output

            --, laTeXState = Reduce.laTeXState tc.parsed state.laTeXState
            , lineNumber = state.lineNumber + (2 + List.length state.blockContents) -- TODO: think about this.  Is it correct?
        }

    else
        { state
            | blockContents = currentLine_ :: state.blockContents
        }


flush : State -> State
flush state =
    let
        input =
            String.join "\n" (List.reverse state.blockContents)

        -- If the remaining input is nontrivial (/= ""), process it and update the state
        newState =
            if input == "" then
                state

            else
                let
                    tc_ : TextCursor Element
                    tc_ =
                        Parser.Driver.parseLoop state.generation state.lineNumber input

                    tc =
                        { tc_ | text = input }

                    --laTeXState =
                    --    Reduce.laTeXState tc.parsed state.laTeXState
                in
                { state
                    | -- laTeXState = laTeXState
                      output = List.reverse (tc :: state.output)
                }
    in
    newState


countLines : List String -> Int
countLines list =
    list |> List.map String.lines |> List.concat |> List.length |> (\x -> x + 1)



-- HELPER (LOOP)


loop : State -> (State -> Step State State) -> State
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b


type Step state a
    = Loop state
    | Done a



-- CLASSIFY LINE


classify : String -> LineType
classify str =
    case P.run lineTypeParser str of
        Ok lt ->
            lt

        Err _ ->
            LTBlank


lineTypeParser =
    P.oneOf [ beginEnvLineParser, endEnvLineParser, textBlockParser, P.succeed LTBlank ]


beginEnvLineParser : P.Parser LineType
beginEnvLineParser =
    P.succeed (\s -> BeginElementBlock)
        |. P.symbol "\\["
        |= P.getChompedString (P.chompUntil "[")


endEnvLineParser : P.Parser LineType
endEnvLineParser =
    P.succeed (\s -> EndElementBlock)
        |. P.symbol "\\]"
        |= P.getChompedString (P.chompUntil "]")


textBlockParser : P.Parser LineType
textBlockParser =
    P.succeed LTTextBlock
        |. P.chompIf (\_ -> True)
