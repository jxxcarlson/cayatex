module Parser.Driver exposing (parseLoop, pl)

import List.Extra
import Parser.Advanced as PA
import Parser.Data as Data exposing (Data)
import Parser.Element as Element exposing (Element(..))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Getters as Getters
import Parser.Loop as Loop
import Parser.Metadata as Metadata exposing (Metadata)
import Parser.TextCursor as TextCursor exposing (TextCursor)


{-| The value of Loop.Packet that we need here
-}
packet : Loop.Packet Element
packet =
    { parser = Element.parser
    , getSource = Getters.getSource
    , incrementOffset = incrementOffset
    , highlighter = Nothing
    , handleError = Just handleError
    }


parseLoop : Int -> Int -> Data -> String -> TextCursor Element
parseLoop generation initialLineNumber data str =
    Loop.parseLoop packet generation initialLineNumber data str


pl : String -> List Element
pl str =
    parseLoop 0 0 (Data.init Data.defaultConfig) str |> .parsed


pl2 str =
    pl str |> List.map Getters.strip


{-| increment the offset field of the SourceMap component of an Expression
-}
incrementOffset : Int -> Element -> Element
incrementOffset delta expr =
    case expr of
        Text s sm ->
            Text s (incrementSourceMapOffset delta sm)

        Element name args body_ sm ->
            Element name args body_ (incrementSourceMapOffset delta sm)

        LX e sm ->
            LX e (incrementSourceMapOffset delta sm)


incrementSourceMapOffset : Int -> Maybe Metadata -> Maybe Metadata
incrementSourceMapOffset delta sourceMap =
    case sourceMap of
        Just sm ->
            Just { sm | offset = sm.offset + delta }

        Nothing ->
            Nothing



-- ERROR HANDLER


type alias ParseError  =
    PA.DeadEnd Context Problem


{-| TODO: Document how this works and how it is extended.
 The role of handleError is to take a textCursor and a list
 of errors and return a corrected textCursor.
-}
handleError : TextCursor Element -> List ParseError -> TextCursor Element
handleError tc errors =
    case List.head errors of
        Nothing -> tc
        Just firstError ->
            let



                problem : Problem
                problem =
                    firstError |> .problem |> Debug.log "PROBLEM"

                errorColumn = firstError.col

                errorRow = firstError.row
                _ = Debug.log "Error (row, col)" (errorRow, errorColumn)

                errorText =
                    -- Example: if the input text is "foo [b bar", then
                    -- the error text is "[b bar"
                    -- But if the input text is "foo [b bar\n\nabc", then
                    -- the error text is "b b" which is INCORRECT.
                    -- String.left errorColumn tc_.text |> Debug.log "ERROR TEXT (0)"
                     tc.text |> Debug.log "ERROR TEXT (0)"


                lxError =
                    Element "Error" [] (Text errorText Nothing) (Just { blockOffset = tc.blockIndex, length = errorColumn, offset = tc.offset + errorColumn, generation = tc.generation, label = "" })
            in
            case problem of
                ExpectingRightBracket ->
                    handleRightBracketError tc firstError errorText

                ExpectingLeftBracket ->
                    handleLeftBracketError tc firstError errorText

                ExpectingPipe ->
                    handlePipeError tc firstError errorText

                _ ->
                    unhandledError tc firstError errorText lxError


unhandledError tc firstError errorText lxError =
    { text = tc.text -- makeNewText tc_ errorColumn mRecoveryData
    , block = ""
    , blockIndex = tc.blockIndex
    , parsand = Nothing
    , parsed = [] -- newParsed tc lxError mRecoveryData
    , stack = [] -- newStack tc errorText mRecoveryData
    , offset = 0 -- newOffset tc errorColumn mRecoveryData
    , count = tc.count
    , generation = tc.generation
    , data = tc.data
    , error = { status = TextCursor.UnhandledError, correctedText = [] }
    }



-- The handlers below will be rationalized and simplified.  Still in an experimental state.


handleRightBracketError : TextCursor Element -> ParseError -> String -> TextCursor Element
handleRightBracketError tc ({row, col} as firstError) errorText =
    let

        --correctedText2 = errorText ++ "]"  |>   Debug.log "CORR. TEXT 1"


        textLines =
            String.lines tc.text |> Debug.log "TEXT LINES"

        errorLines : List String
        errorLines =
            List.take firstError.row textLines

        replacementText =
            "[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| [b " ++ fixUp errorText ++ " ]]"
               |> Debug.log "REPLACEMENT TEXT"

        newTextLines3 = replacementText :: (List.drop 1 textLines)
          |> List.reverse


        newText =
            String.join "\n" newTextLines3
    in
    { text = newText
    , block = ""
    , blockIndex = tc.blockIndex --
    , parsand = Nothing
    , parsed =  tc.parsed
    , stack = [] -- not used
    , offset = 0 -- tc_.offset + firstError.col
    , count = tc.count
    , generation = tc.generation
    , data = tc.data
    , error = { status = TextCursor.RightBracketError, correctedText = newTextLines3 }
    }


fixUp : String -> String
fixUp str =
    str |> String.replace "[" fakeLeftBracket
      |> String.replace "]" fakeRightBracket

handleLeftBracketError : TextCursor Element -> ParseError -> String -> TextCursor Element
handleLeftBracketError tc_ ({row, col} as firstError) errorText  =
    let
        _ = Debug.log "COUNT" tc_.count
        textLines =
            String.lines tc_.text

        badText =
            case List.head textLines of
                Nothing ->
                    "Oops, couldn't find your error text"

                Just str ->
                    str

        name =
            String.replace "[" "" (Debug.log "BLOCK (1)" tc_.block) |> String.words |> List.head |> Maybe.withDefault "NAME"



        errorLines : List String
        errorLines =
            List.take row textLines

        replacementText =
            "[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| " ++ fakeLeftBracket
              ++ " " ++ (Debug.log "NAME" name) ++ " " ++ (Debug.log "BLOCK (2)" tc_.block) ++ " " ++ fakeRightBracket ++ "]"
              |> Debug.log "REPLACEMENT TEXT"

        newTextLines =
            -- ("[highlightRGB |255, 130, 130| missing right bracket in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]") :: List.drop errorRow textLines
            List.Extra.setIf (\t -> t == badText) replacementText errorLines


        newText =
            String.join "\n" (newTextLines)
    in
    { text = newText
    , block = ""
    , blockIndex = tc_.blockIndex --
    , parsand = Nothing
    , parsed =  tc_.parsed -- throw away the erroneous parsand
    , stack = [] -- not used
    , offset = 0 -- newOffset tc_ errorColumn mRecoveryData
    , count = tc_.count
    , generation = tc_.generation
    , data = tc_.data
    , error = { status = TextCursor.LeftBracketError, correctedText = newTextLines }
    }


handlePipeError : TextCursor Element -> ParseError -> String -> TextCursor Element
handlePipeError tc_ ({row, col} as firstError) errorText  =
    let
        textLines : List String
        textLines =
            String.lines tc_.text

        badText : String
        badText =
            case List.head textLines of
                Nothing ->
                    "Oops, couldn't find your error text"

                Just str ->
                    str

        correctedText : String
        correctedText =
            badText
                |> String.replace "[" fakeLeftBracket
                |> String.replace "|" fakePipeSymbol
                |> (\s -> s ++ fakeRightBracket)



        errorLines : List String
        errorLines =
            List.take (row - 1) textLines

        replacementText : String
        replacementText =
            "[highlightRGB |255, 130, 130| missing trailing pipe symbol in] [highlightRGB |186, 205, 255| " ++ correctedText ++ " ]"

        newTextLines : List String
        newTextLines =
            List.Extra.setIf (\t -> t == badText) replacementText textLines |> List.reverse
    in
    { text = String.join "\n" (List.reverse newTextLines)
    , block = ""
    , blockIndex = tc_.blockIndex
    , parsed = parse__ (String.join "\n" errorLines)
    , parsand = Nothing
    , stack = [] --newStack tc_ errorText mRecoveryData
    , offset = 0 -- newOffset tc_ errorColumn mRecoveryData
    , count = tc_.count
    , generation = tc_.generation
    , data = tc_.data
    , error = { status = TextCursor.PipeError, correctedText = newTextLines }
    }



-- HELPERS


newOffset tc_ errorColumn_ mRecoveryData_ =
    case mRecoveryData_ of
        Just rd ->
            tc_.offset + rd.deltaOffset

        Nothing ->
            tc_.offset + errorColumn_


newParsed tc_ lxError_ mRecoveryData =
    case mRecoveryData of
        Just rd ->
            rd.parseSubstitute :: tc_.parsed

        _ ->
            lxError_ :: tc_.parsed


makeNewText tc_ errorColumn_ mRecoveryData =
    case mRecoveryData of
        Just rd ->
            String.dropLeft rd.textTruncation tc_.text

        Nothing ->
            String.dropLeft errorColumn_ tc_.text


newStack tc_ errorText_ mRecoveryData =
    case mRecoveryData of
        Just _ ->
            if List.head tc_.stack == Just "highlight" then
                tc_.stack

            else
                "highlight" :: tc_.stack

        Nothing ->
            errorText_ :: "highlight" :: tc_.stack


parse__ : String -> List Element
parse__ str =
    case PA.run (Element.listParser 0 0) str of
        Ok list ->
            list

        Err _ ->
            -- TODO: vvv very bad code.  Fix this! vvv
            []



-- FAKE CHARACTERS


fakeLeftBracket =
    String.fromChar '⁅'


fakePipeSymbol =
    String.fromChar 'ǀ'


fakeRightBracket =
    String.fromChar '⁆'


listExample =
    """[list

[item Raspberry jam]

[item Sourdough bread]

]
"""
