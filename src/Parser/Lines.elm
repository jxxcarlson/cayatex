module Parser.Lines exposing
    ( process, toParsed
    , Step(..), applyNextState, differentialBlockLevel, getParseResult, initWithDefault, nextState, processWithData, runLoop, toBlocks
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
    loop (initWithDefault generation strList) nextState


runLoopWithData : Int -> Parser.Data.Data -> List String -> State
runLoopWithData generation data strList =
    loop (initWithData generation data strList) nextState


initWithDefault : Int -> List String -> State
initWithDefault generation strList =
    initWithData generation (Parser.Data.init Parser.Data.defaultConfig) strList


initWithData : Int -> Parser.Data.Data -> List String -> State
initWithData generation data strList =
    { input = strList
    , lineNumber = 0
    , generation = generation
    , blockStatus = Start
    , blockContents = []
    , blockLevel = 0
    , blockLevels = { blanksSeen = 0, bracketLevel = 0, textLevel = 0, expectingRaw = NotExpecting }
    , lastTextCursor = Nothing
    , output = []
    , data = data
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
            -- there is a line to process and there are no errors, so let's go for it!
            -- innerNextState currentLine state_
            Loop (innerNextState currentLine state_)


differentialBlockLevel : String -> Int
differentialBlockLevel str =
    List.length (String.indices "[" str) - List.length (String.indices "]" str)


blockFinished : BlockLevels -> Bool
blockFinished { bracketLevel, textLevel } =
    bracketLevel == 0 && textLevel == 0


updateBlockLevels : String -> BlockLevels -> BlockLevels
updateBlockLevels line { blanksSeen, bracketLevel, textLevel } =
    let
        blanksSeen_ =
            if line == "" then
                blanksSeen + 1

            else
                0

        bracketLevel_ =
            differentialBlockLevel line + bracketLevel

        textLevel_ =
            if line == "" then
                0

            else if textLevel == 0 then
                1

            else
                textLevel
    in
    { blanksSeen = blanksSeen_, bracketLevel = bracketLevel_, textLevel = textLevel_, expectingRaw = NotExpecting }


innerNextState : String -> State -> State
innerNextState currentLine state_ =
    let
        state =
            { state_ | input = List.drop 1 state_.input }

        oldBlockLevels =
            state_.blockLevels

        hashesFound =
            String.contains "raw###" currentLine

        expectingRaw =
            if state_.blockLevels.expectingRaw == NotExpecting && hashesFound then
                ExpectingRaw "###"

            else if state_.blockLevels.expectingRaw == ExpectingRaw "###" && hashesFound then
                NotExpecting

            else
                NotExpecting

        newBlockLevels =
            if state_.blockLevels.expectingRaw == NotExpecting then
                updateBlockLevels currentLine oldBlockLevels

            else
                oldBlockLevels
    in
    case ( blockFinished oldBlockLevels, blockFinished newBlockLevels ) of
        ( True, True ) ->
            ignoreLine newBlockLevels state

        ( True, False ) ->
            newBlock currentLine newBlockLevels state

        ( False, True ) ->
            finishBlock currentLine newBlockLevels state

        ( False, False ) ->
            accumulate currentLine newBlockLevels state


accumulate : String -> BlockLevels -> State -> State
accumulate line newBlockLevels state =
    { state
        | blockContents = line :: state.blockContents
        , blockLevels = newBlockLevels
    }


ignoreLine : BlockLevels -> State -> State
ignoreLine newBlockLevels state =
    { state | blockLevels = newBlockLevels, blockContents = [] }


newBlock : String -> BlockLevels -> State -> State
newBlock currentLine newBlockLevels state =
    { state | blockLevels = newBlockLevels, blockContents = [ currentLine ] }


finishBlock : String -> BlockLevels -> State -> State
finishBlock line newBlockLevels state =
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
        , blockLevels = newBlockLevels
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
                    correctedText =
                        Maybe.map .correctedText errorStatus
                            |> Maybe.withDefault [ "Could not correct the error" ]
                            |> List.reverse

                    correctedState =
                        { state
                            | input = correctedText
                            , blockContents = []
                            , blockLevels = { blanksSeen = 0, bracketLevel = 0, textLevel = 0, expectingRaw = NotExpecting }
                        }
                in
                Loop correctedState

            _ ->
                Done newState


countLines : List String -> Int
countLines list =
    list |> List.map String.lines |> List.concat |> List.length |> (\x -> x + 1)



-- ERROR HANDLER


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
                                | blockLevels = { blanksSeen = 0, bracketLevel = 0, textLevel = 0, expectingRaw = NotExpecting }
                                , lastTextCursor = Maybe.map resetError state.lastTextCursor
                            }

                    TextCursor.RightBracketError ->
                        let
                            correctedText =
                                err.correctedText |> List.head |> Maybe.withDefault "Could not get corrected text"

                            foo =
                                1
                        in
                        Loop
                            { state
                                | blockLevels = { blanksSeen = 0, bracketLevel = 0, textLevel = 0, expectingRaw = NotExpecting }
                                , lastTextCursor = Maybe.map resetError state.lastTextCursor
                            }

                    TextCursor.PipeError ->
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

                    _ ->
                        Done state


resetError : TextCursor e -> TextCursor e
resetError tc =
    { tc | error = { status = TextCursor.NoError, correctedText = [] } }



-- GETTERS


getParseResult : Step State State -> List (List Element)
getParseResult stepState =
    case stepState of
        Loop state ->
            state.output |> List.map .parsed |> List.map (List.map Parser.Getters.strip)

        Done state ->
            state.output |> List.map .parsed |> List.map (List.map Parser.Getters.strip)


{-| Return the AST from the State.
-}
toParsed : State -> List (List Element)
toParsed state =
    state.output |> List.map .parsed |> expandErrors |> List.reverse



-- |> Debug.log "AST"


expandErrorF : List Element -> List (List Element)
expandErrorF list =
    if Parser.Element.hasProblem list then
        List.map (\x -> [ x ]) list |> List.reverse

    else
        [ list ]


expandErrors : List (List Element) -> List (List Element)
expandErrors elements =
    elements
        |> List.map expandErrorF
        |> List.concat


{-| Return the AST from the State.
-}
toBlocks : State -> List String
toBlocks state =
    state.output |> List.map .block |> List.reverse



-- FOR TESTING


{-| Used in DocLoopTess
-}
applyNextState : Step State State -> Step State State
applyNextState stepState =
    case stepState of
        Loop state ->
            nextState state

        Done state ->
            Done state



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
