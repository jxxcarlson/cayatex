module Parser.Lines exposing
    ( process, toParsed, toText
    , Step(..), applyNextState, bl, cl, differentialBlockLevel, getParseResult, init, nextState, processWithData, rl, rl_, runLoop, toc
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
then the block offset is 100, the offset of "a" is 0, the offset of "b" is 2, and the offest of "c" is 4.

NOTES:

  - Parser.Driver.parseLoop is called in three places:
      - pushBlock\_ : String -> State -> State
      - popBlockStack : String -> State -> State
      - flush : State -> State
        These are the sites in Document.runLoop at which parsing is done.

-}

import Parser.Data
import Parser.Driver
import Parser.Element exposing (Element(..))
import Parser.Getters
import Parser.Line
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Parser.Types exposing (..)


{-| Compute the syntax tree and LaTeXState of a string of source text.
-}
process : Int -> List String -> State
process generation =
    runLoop generation


processWithData : Int -> Parser.Data.Data -> List String -> State
processWithData generation data =
    runLoopWithData generation data


{-| Compute the final State of a string of source text.
The output field of State holds the AST of the source text.

Function 'process' operates a loop for a state machine which
identifies logical chunks of text, parses these using
Parser.Driver.parseLoop, and prepends them to a list of TextCursor.
The parsed text is held the field 'parsed' of TextCursor.

Each time a loop is completed, the value of Parser.SourceText.SourceText
is updated. The final value will be used in Render.Elm to
furnish section numbering, cross references, a table of contents,
etc.

-}
runLoop : Int -> List String -> State
runLoop generation strList =
    loop (init generation strList) nextState


runLoopWithData : Int -> Parser.Data.Data -> List String -> State
runLoopWithData generation data strList =
    loop (initWithData generation data strList) nextState


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


{-| Return the AST from the State.
-}
toParsed : State -> List (List Element)
toParsed state =
    state.output |> List.map .parsed |> List.reverse


{-| Return the AST from the State.
-}
toBlocks : State -> List String
toBlocks state =
    state.output |> List.map .block |> List.reverse



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
    , blockStatus = Start
    , blockContents = []
    , blockLevel = 0
    , lastTextCursor = Nothing
    , output = []
    , data = Parser.Data.init Parser.Data.defaultConfig
    }


initWithData : Int -> Parser.Data.Data -> List String -> State
initWithData generation data strList =
    { input = strList
    , lineNumber = 0
    , generation = generation
    , blockStatus = Start
    , blockContents = []
    , blockLevel = 0
    , lastTextCursor = Nothing
    , output = []
    , data = data
    }


debug =
    True


getParseResult : Step State State -> List (List Element)
getParseResult stepState =
    case stepState of
        Loop state ->
            state.output |> List.map .parsed |> List.map (List.map Parser.Getters.strip)

        Done state ->
            state.output |> List.map .parsed |> List.map (List.map Parser.Getters.strip)


applyNextState : Step State State -> Step State State
applyNextState stepState =
    case stepState of
        Loop state ->
            nextState state

        Done state ->
            Done state


noError : State -> Bool
noError state =
    let
        err =
            Maybe.map .error state.lastTextCursor
    in
    err == Nothing || Maybe.map .status err == Just TextCursor.NoError


handleError : State -> Step State State
handleError state =
    if state.input == [] then
        flush state

    else
        let
            err_ =
                Maybe.map .error state.lastTextCursor

            _ =
                Maybe.map .error state.lastTextCursor |> Maybe.map .status
        in
        case err_ of
            Nothing ->
                Done state

            Just err ->
                case err.status of
                    TextCursor.LeftBracketError ->
                        let
                            correctedText =
                                err.correctedText |> List.head |> Maybe.withDefault "Could not get corrected text"

                            foo =
                                1
                        in
                        Loop
                            { state
                                | blockStatus = Start
                                , blockLevel = 0
                                , lastTextCursor = Maybe.map resetError state.lastTextCursor
                            }

                    TextCursor.RightBracketError ->
                        let
                            --_ =
                            --    Debug.log "Parser.Lines.handleError, RightBracketError"
                            correctedText =
                                err.correctedText |> List.head |> Maybe.withDefault "Could not get corrected text"

                            foo =
                                1
                        in
                        Loop
                            { state
                                | blockStatus = Start
                                , blockLevel = 0
                                , lastTextCursor = Maybe.map resetError state.lastTextCursor
                            }

                    TextCursor.PipeError ->
                        let
                            --_ =
                            --    Debug.log "Parser.Lines.handleError, RightBracketError"
                            correctedText =
                                err.correctedText |> List.head |> Maybe.withDefault "Could not get corrected text"

                            foo =
                                1
                        in
                        Loop
                            { state
                                | blockStatus = Start
                                , blockLevel = 0
                                , lastTextCursor = Maybe.map resetError state.lastTextCursor
                            }

                    _ ->
                        Done state


resetError : TextCursor e -> TextCursor e
resetError tc =
    { tc | error = { status = TextCursor.NoError, correctedText = [] } }


type alias OO =
    { input : List String
    , lineNumber : Int
    , generation : Int
    , blockStatus : BlockStatus
    , blockContents : List String
    , blockLevel : Int
    , lastTextCursor : Maybe (TextCursor Element)
    , output : List (TextCursor Element)
    , data : Parser.Data.Data
    }


{-| NEXTSTATE
-}
nextState : State -> Step State State
nextState state_ =
    case ( List.head state_.input, noError state_ ) of
        ( Nothing, _ ) ->
            flush state_

        ( _, False ) ->
            handleError state_

        ( Just currentLine, _ ) ->
            let
                state =
                    { state_ | input = List.drop 1 state_.input }
            in
            case ( state.blockStatus, Parser.Line.classify currentLine ) of
                -- COMMENT
                ( _, LTComment ) ->
                    Loop { state | input = List.drop 1 state.input }

                -- START
                ( Start, LTBlank ) ->
                    Loop (start state)

                ( Start, LTBeginElement ) ->
                    Loop (startBlock currentLine { state | blockStatus = InElementBlock })

                ( Start, LTEndElement ) ->
                    Loop (initBlock InTextBlock ("Error: " ++ currentLine) state)

                ( Start, LTTextBlock ) ->
                    Loop (initBlock InTextBlock currentLine state)

                -- TEXTBLOCK
                ( InTextBlock, LTBlank ) ->
                    -- Then end of a text block has been reached. Create a string representing
                    -- this block, parse it using Parser.parseLoop to produce a TextCursor, and
                    -- add it to state.output.  Finally, update the laTeXState using Render.Reduce.latexState
                    Loop (pushBlock state)

                ( InTextBlock, LTBeginElement ) ->
                    Loop (startBlock currentLine state)

                ( InTextBlock, LTEndElement ) ->
                    Loop (addToBlockContents currentLine state)

                ( InTextBlock, LTTextBlock ) ->
                    Loop (addToBlockContents currentLine state)

                --- ELEMENT BLOCK
                ( InElementBlock, LTBlank ) ->
                    Loop (pushBlockStack currentLine state)

                ( InElementBlock, LTBeginElement ) ->
                    Loop (startBlock currentLine state)

                ( InElementBlock, LTEndElement ) ->
                    Loop (popBlockStack currentLine state)

                ( InElementBlock, LTTextBlock ) ->
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


{-| (ST 1) Put State in the Start state
Used in ( Start, LTBlank )
ST uses: 10 uses, 5 functions in 3x4 + 1 state transitions
-}
start : State -> State
start state =
    { state | blockStatus = Start, blockLevel = 0, blockContents = [] }


{-| (ST 2) Two uses: ( Start, LTEndElement ) and ( Start, LTTextBlock )
-}
initBlock : BlockStatus -> String -> State -> State
initBlock blockType_ currentLine_ state =
    { state | blockStatus = blockType_, blockContents = [ currentLine_ ] }


{-| (ST 3) Three uses: ( InTextBlock, LTEndElement ),( InTextBlock, LTTextBlock ), ( InElementBlock, LTTextBlock )
-}
addToBlockContents : String -> State -> State
addToBlockContents currentLine_ state =
    let
        deltaBlockLevel =
            differentialBlockLevel currentLine_

        newBlockLevel =
            state.blockLevel + deltaBlockLevel |> max 0
    in
    if newBlockLevel == 0 && deltaBlockLevel < 0 then
        pushBlock_ ("\n" ++ currentLine_) state

    else
        { state | blockLevel = newBlockLevel, blockContents = currentLine_ :: state.blockContents }


{-| (ST 4) One use: ( InElementBlock, LTBlank )
-}
pushBlockStack : String -> State -> State
pushBlockStack currentLine_ state =
    let
        deltaBlockLevel =
            differentialBlockLevel currentLine_

        newBlockLevel =
            state.blockLevel + deltaBlockLevel |> max 0
    in
    if newBlockLevel == 0 then
        pushBlock_ ("\n" ++ currentLine_) state

    else
        { state
            | blockContents = currentLine_ :: state.blockContents
            , blockLevel = newBlockLevel
        }


{-| (ST 5) Three uses: ( Start, LTBeginElement ), ( InTextBlock, LTBeginElement ), ( InElementBlock, LTBeginElement )
-}
startBlock : String -> State -> State
startBlock currentLine_ state =
    let
        deltaBlockLevel =
            differentialBlockLevel currentLine_

        newBlockLevel =
            state.blockLevel + deltaBlockLevel |> max 0
    in
    { state
        | blockContents = currentLine_ :: state.blockContents
        , blockLevel = newBlockLevel
        , blockStatus = InElementBlock
    }


{-| (ST 6) Called at ( InTextBlock, LTBlank )
-}
pushBlock : State -> State
pushBlock state =
    pushBlock_ "" state


{-| (ST 6)
-}
pushBlock_ : String -> State -> State
pushBlock_ line state =
    let
        str =
            String.join "\n" (List.reverse state.blockContents)
                ++ "\n"
                ++ line

        tc : TextCursor Element
        tc =
            -- TODO: is usage of state.data correct?
            Parser.Driver.parseLoop state.generation state.lineNumber state.data str
    in
    { state
        | blockStatus = Start
        , blockContents = []
        , blockLevel = 0
        , data = updateData tc
        , lastTextCursor = Just tc
        , output = tc :: state.output
        , lineNumber = state.lineNumber + countLines state.blockContents
    }


updateData tc =
    case tc.parsand of
        Nothing ->
            tc.data

        Just parsand ->
            Parser.Data.update parsand tc.data


{-| (ST 7 Called at ( InElementBlock, LTEndElement )
-}
popBlockStack : String -> State -> State
popBlockStack currentLine_ state =
    let
        newBlockLevel =
            state.blockLevel + differentialBlockLevel currentLine_ |> max 0
    in
    if newBlockLevel == 0 then
        let
            input_ =
                String.join "\n" (List.reverse (currentLine_ :: state.blockContents))

            tc_ =
                -- TODO: is usage of state.data correct?
                Parser.Driver.parseLoop state.generation state.lineNumber state.data input_

            tc =
                { tc_ | text = input_ }
        in
        { state
            | blockStatus = Start
            , blockLevel = 0
            , blockContents = currentLine_ :: state.blockContents
            , lastTextCursor = Just tc
            , output = tc :: state.output
            , data = updateData tc
            , lineNumber = state.lineNumber + (2 + List.length state.blockContents) -- TODO: think about this.  Is it correct?
        }

    else
        { state
            | blockContents = currentLine_ :: state.blockContents
            , blockLevel = newBlockLevel
        }


flush : State -> Step State State
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
                        -- TODO: is usage of state.data correct?
                        Parser.Driver.parseLoop state.generation state.lineNumber state.data input

                    tc =
                        { tc_ | text = input }

                    --laTeXState =
                    --    Reduce.laTeXState tc.parsed state.laTeXState
                in
                { state
                    | -- laTeXState = laTeXState
                      lastTextCursor = Just tc
                    , output = tc :: state.output
                    , data = updateData tc
                }
    in
    let
        errorStatus : Maybe TextCursor.ParseError
        errorStatus =
            Maybe.map .error (List.head newState.output)
    in
    if noError newState then
        Done newState

    else
        case Maybe.map .status errorStatus of
            Just TextCursor.RightBracketError ->
                let
                    --_ =
                    --    Debug.log "Parser.Lines.FLUSH, RightBracketError"
                    correctedText =
                        Maybe.map .correctedText errorStatus |> Maybe.withDefault [ "Could not correct the error" ] |> List.reverse

                    correctedState =
                        { state | input = correctedText, blockContents = [], blockLevel = 0, blockStatus = Start }
                in
                Loop correctedState

            _ ->
                Done newState


countLines : List String -> Int
countLines list =
    list |> List.map String.lines |> List.concat |> List.length |> (\x -> x + 1)



-- HELPER (LOOP)


loop : State -> (State -> Step State State) -> State
loop s nextState_ =
    -- TODO: Uncomment for debugging
    --let
    --    _ =
    --        Debug.log (String.fromInt s.lineNumber) { inp = s.input, err = Maybe.map .error s.lastTextCursor, bt = s.blockStatus, bl = s.blockLevel, bc = s.blockContents }
    --in
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b


type Step state a
    = Loop state
    | Done a
