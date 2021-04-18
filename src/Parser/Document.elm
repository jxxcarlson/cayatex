module Parser.Document exposing
    ( process, toParsed, toText
    , State, Block, BlockType(..), LineType(..)
    , classify, differentialBlockLevel, run
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
at which a block begins. Recall that a block is a string (which may contain newlines) derived
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
import Parser.Getters
import Parser.TextCursor as TextCursor exposing (TextCursor)


{-| -}
type alias State =
    { input : List String
    , lineNumber : Int
    , generation : Int
    , blockType : BlockType
    , blockContents : List String
    , blockLevel : Int
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
    | LTBeginElement
    | LTEndElement
    | LTComment


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
    state.output |> List.map .parsed |> List.reverse



-- |> List.reverse


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
    , blockLevel = 0
    , output = []

    --  , laTeXState = LaTeXState.init
    }


debug =
    True


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
                -- COMMMENT
                ( _, LTComment ) ->
                    Loop { state | input = List.drop 1 state.input }

                -- START
                ( Start, LTBlank ) ->
                    Loop (start state)

                ( Start, LTBeginElement ) ->
                    -- TODO: correct this transition
                    Loop (startBlock currentLine { state | blockType = ElementBlock })

                ( Start, LTEndElement ) ->
                    Loop (initBlock ErrorBlock currentLine state)

                ( Start, LTTextBlock ) ->
                    Loop (initBlock TextBlock currentLine state)

                -- ERROR BLOCK
                ( ErrorBlock, LTBlank ) ->
                    Loop { state | blockType = Start, blockContents = [] }

                ( ErrorBlock, LTBeginElement ) ->
                    Loop (initWithBlockType currentLine state)

                ( ErrorBlock, LTEndElement ) ->
                    Loop (addToBlockContents currentLine state)

                ( ErrorBlock, LTTextBlock ) ->
                    Loop (initWithBlockType currentLine state)

                -- TEXTBLOCK
                ( TextBlock, LTBlank ) ->
                    -- Then end of a text block has been reached. Create a string representing
                    -- this block, parse it using Parser.parseLoop to produce a TextCursor, and
                    -- add it to state.output.  Finally, update the laTeXState using Render.Reduce.latexState
                    Loop (pushBlock state)

                ( TextBlock, LTBeginElement ) ->
                    -- TODO: is this transition correct?
                    -- Loop (initWithBlockType currentLine state)
                    Loop (startBlock currentLine state)

                ( TextBlock, LTEndElement ) ->
                    Loop (addToBlockContents currentLine state)

                ( TextBlock, LTTextBlock ) ->
                    Loop (addToBlockContents currentLine state)

                --- ELEMENT BLOCK
                ( ElementBlock, LTBlank ) ->
                    -- TODO: is this the correct transition? (I think it is OK)
                    Loop (pushBlockStack currentLine state)

                ( ElementBlock, LTBeginElement ) ->
                    -- TODO: is this the correct transition? (I think it is NOT OK)
                    -- Loop (pushBlockStack currentLine state)
                    Loop (startBlock currentLine state)

                ( ElementBlock, LTEndElement ) ->
                    Loop (popBlockStack currentLine state)

                ( ElementBlock, LTTextBlock ) ->
                    Loop (addToBlockContents currentLine state)



-- OPERATIONS ON STATE


differentialBlockLevel : String -> Int
differentialBlockLevel str =
    let
        chars =
            String.split "" str

        leftBrackets =
            List.filter (\s -> s == "[") chars |> List.length

        rightBrackets =
            List.filter (\s -> s == "]") chars |> List.length
    in
    leftBrackets - rightBrackets


{-| Put State in the Start state
-}
start : State -> State
start state =
    { state | blockType = Start, blockLevel = 0, blockContents = [] }


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
        | -- blockContents = [ currentLine_ ]
          blockContents = []
        , lineNumber = state.lineNumber + countLines state.blockContents

        --, laTeXState = laTeXState
        , output = newTC :: state.output
    }


addToBlockContents : String -> State -> State
addToBlockContents currentLine_ state =
    -- TODO: work on this
    let
        deltaBlockLevel =
            differentialBlockLevel currentLine_

        newBlockLevel =
            state.blockLevel + deltaBlockLevel

        _ =
            Debug.log "(BL, Delta)" ( state.blockLevel, deltaBlockLevel )
    in
    if newBlockLevel == 0 && deltaBlockLevel < 0 then
        pushBlock_ ("\n" ++ currentLine_) state

    else
        { state | blockContents = currentLine_ :: state.blockContents }


pushBlockStack : String -> State -> State
pushBlockStack currentLine_ state =
    let
        deltaBlockLevel =
            differentialBlockLevel currentLine_

        newBlockLevel =
            state.blockLevel + deltaBlockLevel
    in
    if newBlockLevel == 0 then
        pushBlock_ ("\n" ++ currentLine_) state

    else
        { state
            | blockContents = currentLine_ :: state.blockContents
            , blockLevel = newBlockLevel
        }


startBlock : String -> State -> State
startBlock currentLine_ state =
    let
        deltaBlockLevel =
            differentialBlockLevel currentLine_

        newBlockLevel =
            state.blockLevel + deltaBlockLevel
    in
    --if newBlockLevel == 0 then
    --    pushBlock_ ("\n" ++ currentLine_) state
    --
    --else
    { state
        | blockContents = currentLine_ :: state.blockContents
        , blockLevel = newBlockLevel
        , blockType = ElementBlock
    }


pushBlock : State -> State
pushBlock state =
    pushBlock_ "" state


pushBlock_ : String -> State -> State
pushBlock_ line state =
    let
        str =
            String.join "\n" (List.reverse state.blockContents) ++ "\n" ++ line

        -- |> Debug.log "PUSH"
        tc : TextCursor Element
        tc =
            Parser.Driver.parseLoop state.generation state.lineNumber str
    in
    { state
        | blockType = Start
        , blockContents = []
        , blockLevel = 0

        -- , laTeXState = Reduce.laTeXState tc.parsed state.laTeXState
        , output = tc :: state.output
        , lineNumber = state.lineNumber + countLines state.blockContents
    }


popBlockStack : String -> State -> State
popBlockStack currentLine_ state =
    let
        newBlockLevel =
            state.blockLevel + differentialBlockLevel currentLine_
    in
    if newBlockLevel == 0 then
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
            , blockLevel = 0
            , blockContents = currentLine_ :: state.blockContents
            , output = tc :: state.output

            --, laTeXState = Reduce.laTeXState tc.parsed state.laTeXState
            , lineNumber = state.lineNumber + (2 + List.length state.blockContents) -- TODO: think about this.  Is it correct?
        }

    else
        { state
            | blockContents = currentLine_ :: state.blockContents
            , blockLevel = newBlockLevel
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
                      -- output = List.reverse (tc :: state.output)
                      output = tc :: state.output
                }
    in
    newState


countLines : List String -> Int
countLines list =
    list |> List.map String.lines |> List.concat |> List.length |> (\x -> x + 1)



-- HELPER (LOOP)


loop : State -> (State -> Step State State) -> State
loop s nextState_ =
    -- TODO: Uncomment for debugging
    --let
    --    _ =
    --        Debug.log (String.fromInt s.lineNumber) { inp = s.input, cl = Maybe.map classify (List.head s.input), bt = s.blockType, bl = s.blockLevel, bc = s.blockContents }
    --in
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
    P.oneOf [ commentParser, beginElementParser, endElementParser, textBlockParser, P.succeed LTBlank ]


beginElementParser : P.Parser LineType
beginElementParser =
    P.succeed (\s -> LTBeginElement)
        |= P.symbol "["


commentParser : P.Parser LineType
commentParser =
    P.succeed (\s -> LTComment)
        |= P.symbol "%"



--|= P.getChompedString (P.chompUntil "[")


endElementParser : P.Parser LineType
endElementParser =
    P.succeed (\s -> LTEndElement)
        |= P.symbol "]"


textBlockParser : P.Parser LineType
textBlockParser =
    P.succeed LTTextBlock
        |. P.chompIf (\_ -> True)
