module Parser.Data exposing (..)

{- (Data, defaultConfig, init, nullUpdate) -}

import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Attributes as HA
import Maybe.Extra
import Parser.Element as Element exposing (Element(..))
import Parser.Function
import Render.Utility
import Vector exposing (Vector)


{-| SourceText is a composite structure holding information on section and other
counters, cross-references, table of contents, etc. It is compute
by Parser.Lines.runLoop and is used by Render.Elm.renderElement,
where it appears as a field in RenderArgs
-}
type alias Data =
    { counters : IntegerDict
    , vectorCounters : VectorDict
    , crossReferences : Dictionary
    , tableOfContents : List TocEntry
    , dictionary : Dictionary
    , macroDict : MacroDict
    , bindings : Bindings
    , config : Config
    }


type alias Bindings =
    Dict String String


type alias MacroForm =
    { name : String, args : List String }


type alias MacroDict =
    Dict String MacroForm


init : Config -> Data
init config =
    { counters = Dict.empty
    , vectorCounters = Dict.fromList [ ( "section", Vector.init 6 ) ]
    , crossReferences = Dict.empty
    , tableOfContents = []
    , dictionary = Dict.empty
    , macroDict = Dict.empty
    , bindings = Dict.empty
    , config = config
    }


type alias IntegerDict =
    Dict.Dict String Int


type alias VectorDict =
    Dict.Dict String Vector


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
    , displayWidth : Int
    }


update : Element -> Data -> Data
update element data =
    case element of
        Element name args body _ ->
            if String.left 7 name == "section" then
                handleSection name body data

            else
                case name of
                    "macro" ->
                        handleMacro name body data

                    "set" ->
                        handleLet name body data

                    "set_" ->
                        handleLet name body data

                    _ ->
                        data

        LX list _ ->
            List.foldl update data list

        _ ->
            data


updateList : List Element -> Data -> Data
updateList list data =
    List.foldl update data list


handleSection : String -> Element -> Data -> Data
handleSection name element data =
    let
        name_ =
            if name == "section" then
                "section1"

            else
                name

        level_ =
            String.toInt (String.replace "section" "" name_)
    in
    case level_ of
        Nothing ->
            data

        Just level ->
            let
                maybeInc =
                    Maybe.map (\x -> Vector.increment (level - 1) x)

                newVectorCounters =
                    Dict.update "section" maybeInc data.vectorCounters

                newData =
                    { data | vectorCounters = newVectorCounters }

                tocItem =
                    { name = getText element, label = sectionLabel newData, level = level - 1 }
            in
            { newData | tableOfContents = tocItem :: data.tableOfContents }


handleLet : String -> Element -> Data -> Data
handleLet name body data =
    { data | bindings = handleLet_ name body data.bindings }


handleLet_ : String -> Element -> Bindings -> Bindings
handleLet_ name body bindings =
    let
        data =
            getText2 body

        parts =
            String.split "," data

        kvList : List ( String, String )
        kvList =
            List.map (String.split "=" >> List.map String.trim >> makePair) parts
                |> Maybe.Extra.values
    in
    List.foldl (\( k, v ) -> Dict.insert k v) bindings kvList


makePair : List String -> Maybe ( String, String )
makePair ns =
    case ns of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


getText2 : Element -> String
getText2 element =
    case element of
        LX list_ _ ->
            List.map Render.Utility.extractText list_ |> Maybe.Extra.values |> String.join "\n"

        _ ->
            ""


handleMacro : String -> Element -> Data -> Data
handleMacro str element data =
    { data | macroDict = handleMacro_ str element data.macroDict }


handleMacro_ : String -> Element -> MacroDict -> MacroDict
handleMacro_ _ element dict =
    case element of
        LX [ Element macroName_ _ (LX [ Element name_ args_ _ _ ] _) _ ] _ ->
            Dict.insert macroName_ { name = name_, args = args_ } dict

        _ ->
            dict



--LX
--[ Element "blue" [] (LX [ Element "fontRGB" [ "0", "80", "200" ]
--   (LX [ Text "body" (Just { blockOffset = 2, generation = 2, label = "", length = 4, offset = 35 }) ] Nothing) (Just { blockOffset = 2, generation = 2, label = "", length = 27, offset = 13 }) ] Nothing) (Just { blockOffset = 2, generation = 2, label = "", length = 34, offset = 7 }) ]
--Nothing


getText : Element -> String
getText element =
    case element of
        LX list_ _ ->
            List.map Render.Utility.extractText list_ |> Maybe.Extra.values |> String.join "\n"

        _ ->
            ""


labelElement : Data -> Element -> Element
labelElement data element =
    case element of
        Element name args body _ ->
            if String.left 7 name == "section" then
                setSectionLabel name data element

            else
                element

        _ ->
            element


sectionLabel data =
    Dict.get "section" data.vectorCounters |> Maybe.map Vector.toString |> Maybe.withDefault "??"


setSectionLabel : String -> Data -> Element -> Element
setSectionLabel name_ data element =
    Parser.Function.setLabel (sectionLabel data) element



-- DEFAULTS


nullUpdate : a -> Data -> Data
nullUpdate a data =
    data


defaultConfig : Config
defaultConfig =
    { redColor = "#a00"
    , blueColor = "#00c"
    , highlightColor = "#fAA"
    , displayWidth = 470
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
