module Parser.Data exposing (..)

{- (Data, defaultConfig, init, nullUpdate) -}

import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Attributes as HA
import Parser.Element as Element exposing (Element(..))


{-| SourceText is a composite strucure holding information on section and other
counters, cross-references, table of contents, etc. It is compute
by Parser.Document.runLoop and is used by Render.Elm.renderElement,
where it appears as a field in RenderArgs
-}
type alias Data =
    { counters : IntegerDict
    , crossReferences : Dictionary
    , tableOfContents : List TocEntry
    , dictionary : Dictionary
    , config : Config
    }


init : Config -> Data
init config =
    { counters = initialCounters
    , crossReferences = Dict.empty
    , tableOfContents = []
    , dictionary = Dict.empty
    , config = config
    }


type alias IntegerDict =
    Dict.Dict String Int


{-| -}
type alias TableOfContents =
    List TocEntry


{-| -}
type alias TocEntry =
    { name : String, label : String, level : Int }


type alias Dictionary =
    Dict String String


type alias Config =
    { redColor : String
    , blueColor : String
    , highlightColor : String
    }


update : Element -> Data -> Data
update element data =
    case element of
        Element name args body _ ->
            if String.left 7 name == "section" then
                handleSection name data

            else
                data

        LX list _ ->
            List.foldl update data list

        _ ->
            data


updateList : List Element -> Data -> Data
updateList list data =
    List.foldl update data list


handleSection : String -> Data -> Data
handleSection name data =
    let
        name_ =
            if name == "section" then
                "section1"

            else
                name
    in
    incrementOrInsertCounter name_ data


labelElement : Data -> Element -> Element
labelElement data element =
    case element of
        Element name args body _ ->
            if String.left 7 name == "section" then
                -- Element.setLabel (String.fromInt (getCounter name data)) element
                setSectionLabel name data element

            else
                element

        _ ->
            element


setSectionLabel : String -> Data -> Element -> Element
setSectionLabel name_ data element =
    let
        name =
            if name_ == "section" then
                "section1"

            else
                name_
    in
    case name of
        "section1" ->
            Element.setLabel (String.fromInt (getCounter "section1" data)) element

        "section2" ->
            let
                label_ =
                    String.fromInt (getCounter "section1" data) ++ "." ++ String.fromInt (getCounter "section2" data)
            in
            Element.setLabel label_ element

        _ ->
            Element.setLabel "??" element



-- DEFAULTS


nullUpdate : a -> Data -> Data
nullUpdate a data =
    data


defaultConfig : Config
defaultConfig =
    { redColor = "#a00"
    , blueColor = "#00c"
    , highlightColor = "#fAA"
    }



-- HELPERS


{-| -}
addSection : String -> String -> Int -> Data -> Data
addSection sectionName label level data =
    let
        newEntry =
            TocEntry sectionName label level

        toc =
            data.tableOfContents ++ [ newEntry ]
    in
    { data | tableOfContents = toc }


{-| Return the value of a named counter from the LaTeXSTate
-}
getCounter : String -> Data -> Int
getCounter name state =
    case Dict.get name state.counters of
        Just k ->
            k

        Nothing ->
            0


{-| -}
getCrossReference : String -> Data -> String
getCrossReference label state =
    case Dict.get label state.crossReferences of
        Just ref ->
            ref

        Nothing ->
            "??"


{-| -}
getDictionaryItem : String -> Data -> String
getDictionaryItem key state =
    case Dict.get key state.dictionary of
        Just value ->
            value

        Nothing ->
            ""


{-| -}
setDictionaryItem : String -> String -> Data -> Data
setDictionaryItem key value state =
    let
        dictionary =
            state.dictionary

        newDictionary =
            Dict.insert key value dictionary
    in
    { state | dictionary = newDictionary }


{-| -}
insertCounter : String -> Int -> Data -> Data
insertCounter name value state =
    let
        maybeInc =
            Maybe.map (\x -> x + 1)

        newCounters =
            Dict.insert name value state.counters
    in
    { state | counters = Dict.insert name value state.counters }


{-| -}
incrementCounter : String -> Data -> Data
incrementCounter name state =
    let
        maybeInc =
            Maybe.map (\x -> x + 1)

        newCounters =
            Dict.update name maybeInc state.counters
    in
    { state | counters = newCounters }


incrementOrInsertCounter : String -> Data -> Data
incrementOrInsertCounter name state =
    let
        maybeInc =
            Maybe.map (\x -> x + 1)

        newCounters =
            case Dict.get name state.counters of
                Nothing ->
                    Dict.insert name 1 state.counters

                Just k ->
                    Dict.insert name (k + 1) state.counters
    in
    { state | counters = newCounters }


{-| -}
setCounter : String -> Int -> Data -> Data
setCounter name value state =
    let
        maybeSet =
            Maybe.map (\_ -> value)

        newCounters =
            Dict.update name maybeSet state.counters
    in
    { state | counters = newCounters }


{-| -}
setCrossReference : String -> String -> Data -> Data
setCrossReference label value state =
    let
        crossReferences =
            state.crossReferences

        newCrossReferences =
            Dict.insert label value crossReferences
    in
    { state | crossReferences = newCrossReferences }


initialCounters : Dict String Int
initialCounters =
    Dict.fromList []
