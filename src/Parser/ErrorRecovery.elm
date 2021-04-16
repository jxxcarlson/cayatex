module Parser.ErrorRecovery exposing (..)

import Parser.Element as Element exposing (Element(..))
import Parser.Error exposing (Problem(..))
import Parser.SourceMap as SourceMap exposing (SourceMap)
import Parser.TextCursor exposing (TextCursor)


{-| -}
type alias RecoveryData =
    { problem : Problem
    , deltaOffset : Int
    , textTruncation : Int
    , parseSubstitute : Element
    }


{-| -}
getRecoveryData : TextCursor Element -> Problem -> Maybe RecoveryData
getRecoveryData tc_ problem =
    let
        oldSourceMap =
            SourceMap.dummy

        newSourceMap =
            Just { oldSourceMap | blockOffset = tc_.blockIndex }
    in
    getRecoveryData_ problem
        |> Maybe.map (\r -> { r | parseSubstitute = setSourceMap newSourceMap r.parseSubstitute })


getRecoveryData_ : Problem -> Maybe RecoveryData
getRecoveryData_ problem =
    List.filter (\r -> Element.equivalentProblem r.problem problem) recoveryData |> List.head


recoveryData : List RecoveryData
recoveryData =
    [ problemWithInlineMath, problemWithDisplayMath, problemWithEnvironment, problemWithMacro ]


problemWithInlineMath : RecoveryData
problemWithInlineMath =
    { problem = ExpectingTrailingDollarSign
    , deltaOffset = 1
    , textTruncation = 1
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text (String.fromChar '⚠' ++ " unmatched $ in" ++ String.fromChar '\u{00A0}') Element.dummySourceMap
                ]
                Element.dummySourceMap
            ]
    }


problemWithDisplayMath : RecoveryData
problemWithDisplayMath =
    { problem = ExpectingTrailingDoubleDollarSign
    , deltaOffset = 2
    , textTruncation = 2 -- corresponds to "$$"
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text (String.fromChar '⚠' ++ " unmatched $$ in" ++ String.fromChar '\u{00A0}') Element.dummySourceMap
                ]
                Element.dummySourceMap
            ]
    }


problemWithMacro : RecoveryData
problemWithMacro =
    { problem = ExpectingRightBrace
    , deltaOffset = 0
    , textTruncation = 1
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text "!! missing right brace in \\"
                    Element.dummySourceMap
                ]
                Element.dummySourceMap
            ]
    }


problemWithEnvironment : RecoveryData
problemWithEnvironment =
    { problem = ExpectingEndWord "dummy"
    , deltaOffset = 3
    , textTruncation = 2
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ DisplayMath "!! unmatched \\begin .. \\end: " Element.dummySourceMap
                ]
                Element.dummySourceMap
            ]
    }



-- ERRORS


{-| getErrors takes parsed text as input and produces a list of errors as output.
-}
getErrors : List (List Element) -> List Element
getErrors list =
    list
        |> List.map getErrors_
        |> List.concat


getErrors_ : List Element -> List Element
getErrors_ list =
    List.filter (\e -> isError e) list


isError : Element -> Bool
isError expr =
    case expr of
        LXError _ _ _ ->
            True

        _ ->
            False



-- HELPERS


{-| -}
setSourceMap : Maybe SourceMap -> Element -> Element
setSourceMap sm expr =
    case expr of
        Text e _ ->
            Text e sm

        Element name args body _ ->
            Element name args body sm

        LX list _ ->
            LX list sm



--type Element
--    = Text String (Maybe SourceMap)
--    | Element String (List String) Element (Maybe SourceMap)
--    | LX (List Element) (Maybe SourceMap)
