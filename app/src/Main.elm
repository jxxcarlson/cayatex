module Main exposing (keyedNode, main)

import Browser
import CaYaTeX
import Data
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Keyed
import Html.Parser
import Html.Parser.Util
import Paragraph
import Parser.Data
import Parser.Element as Parser
import Parser.Getters
import Parser.Lines
import Render.Elm
import Render.String


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , renderedText : String
    , mode : Mode
    , count : Int
    , windowHeight : Int
    , windowWidth : Int
    }


type Msg
    = NoOp
    | InputText String
    | ClearText
    | GetDocument String
    | SetMode Mode
    | CYTMsg Parser.CYTMsg


type Mode
    = RawHTMLMode
    | RenderedMode


type alias Flags =
    { width : Int, height : Int }


initialText =
    Data.get "announcement"


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = initialText
      , renderedText = Render.String.renderString initialText
      , mode = RenderedMode
      , count = 0
      , windowHeight = flags.height
      , windowWidth = flags.width
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model
                | input = str
                , renderedText = Render.String.renderString str
                , count = model.count + 1
              }
            , Cmd.none
            )

        SetMode mode ->
            ( { model | mode = mode, count = model.count + 1 }, Cmd.none )

        ClearText ->
            ( { model
                | input = ""
                , renderedText = Render.String.renderString ""
                , count = model.count + 1
              }
            , Cmd.none
            )

        GetDocument docName ->
            ( { model
                | input = Data.get docName
                , renderedText = Render.String.renderString (Data.get docName)
                , count = model.count + 1
              }
            , Cmd.none
            )

        CYTMsg _ ->
            ( model, Cmd.none )



--
-- VIEW
--


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle noFocus ] } [ bgGray 0.2, clipX, clipY ] (mainColumn model)


noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }



-- PARAMETERS


widePanelWidth_ =
    2 * panelWidth_


panelWidth_ =
    520


appHeight_ model =
    model.windowHeight - 300


panelHeight_ model =
    appHeight_ model - parserDisplayPanelHeight_ - 100


parserDisplayPanelHeight_ =
    0


appWidth_ =
    2 * panelWidth_ + 15


mainColumn : Model -> Element Msg
mainColumn model =
    column (mainColumnStyle model)
        [ column [ spacing 48, width (px appWidth_), height (px (appHeight_ model)) ]
            [ title "CaYaTeX Test App"
            , column [ spacing 12 ]
                [ row [ spacing 12 ] [ inputElement model, outputDisplay model ]

                -- , parserDisplay model
                ]
            , row [ Font.size 14, Font.color whiteColor ] []
            ]
        ]


inputElement model =
    column [ spacing 8, moveUp 9 ]
        [ row [ spacing 12 ] [ clearTextButton, getDocumentButton "announcement", getDocumentButton "notes", getDocumentButton "manual" ]
        , inputText model
        ]


title : String -> Element msg
title str =
    row [ centerX, fontGray 0.9 ] [ text str ]


parserDisplay model =
    row
        [ moveUp 10
        , Font.size 14
        , Background.color whiteColor
        , padding 8
        ]
        [ parsed model ]


parsed model =
    case Parser.parseList model.count 0 model.input |> Result.map (List.map Parser.Getters.strip) of
        Err _ ->
            text "Parse error"

        Ok pt ->
            el [ alignTop ]
                (column
                    [ width (px widePanelWidth_)
                    , height (px parserDisplayPanelHeight_)
                    , scrollbarY
                    ]
                    (List.map (\s -> Element.paragraph [] [ text s ]) (parsed_ pt))
                )


parsed_ : a -> List String
parsed_ pt =
    -- Paragraph.lines paragraphFormat2 (Debug.toString pt)
    []


outputDisplay : Model -> Element Msg
outputDisplay model =
    column [ spacing 8 ]
        [ row
            [ fontGray 0.9
            , spacing 12
            , moveUp 9
            , Font.size 14
            ]
            [ dummyButton, text ("generation: " ++ String.fromInt model.count), wordCountElement model.input ]
        , outputDisplay_ model
        ]


wordCount : String -> Int
wordCount str =
    str |> String.words |> List.length


wordCountElement : String -> Element Msg
wordCountElement str =
    row [ spacing 8 ] [ el [] (text <| "words:"), el [] (text <| String.fromInt <| wordCount <| str) ]


outputDisplay_ : Model -> Element Msg
outputDisplay_ model =
    column
        [ spacing 8
        , Background.color (Element.rgb 1.0 1.0 1.0)
        , paddingXY 24 36
        , width (px panelWidth_)
        , height (px (panelHeight_ model))
        , scrollbarY
        , moveUp 9
        , Font.size 12
        ]
        (if model.mode == RenderedMode then
            [ render model.count model.input ]

         else
            List.map text (Paragraph.lines paragraphFormat model.renderedText)
        )



-- RENDER STRING TO HTML MSG


initState k =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = Parser.Data.init Parser.Data.defaultConfig
    }


initStateWithData k data =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = data
    }


render : Int -> String -> Element Msg
render k str =
    -- CaYaTeX.render "id__" { content = str, generation = k } |> Element.map CYTMsg
    let
        state =
            Parser.Lines.runLoop k (String.lines str)

        newState =
            initStateWithData k state.data
    in
    state
        |> Parser.Lines.toParsed
        |> List.map (Render.Elm.renderList newState)
        |> column [ spacing 18 ]
        |> Element.map CYTMsg


paragraphFormat =
    { maximumWidth = 80, optimalWidth = 70, stringWidth = String.length }


paragraphFormat2 =
    { maximumWidth = 160, optimalWidth = 150, stringWidth = String.length }


keyedNode : Int -> List Html.Parser.Node -> Element msg
keyedNode k element =
    Html.Parser.Util.toVirtualDom element
        |> keyIt k
        |> Html.Keyed.node "div" []
        |> Element.html


keyIt : Int -> List b -> List ( String, b )
keyIt k list =
    List.indexedMap (\i e -> ( String.fromInt (i + k), e )) list



-- INPUT


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ height (px (panelHeight_ model)), width (px panelWidth_), Font.size 14 ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing

        --, label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Source text")
        , label = Input.labelHidden "Enter source text here"
        , spellcheck = False
        }



-- BUTTONS


defaultButtonColor =
    Element.rgb255 60 60 60


buttonColor buttonMode currentMode =
    if buttonMode == currentMode then
        Element.rgb255 130 12 9

    else
        Element.rgb255 60 60 60


clearTextButton : Element Msg
clearTextButton =
    Input.button buttonStyle2
        { onPress = Just ClearText
        , label = el [ centerX, centerY, Font.size 14 ] (text "clear")
        }


getDocumentButton : String -> Element Msg
getDocumentButton docName =
    Input.button buttonStyle2
        { onPress = Just (GetDocument docName)
        , label = el [ centerX, centerY, Font.size 14 ] (text docName)
        }


dummyButton : Element Msg
dummyButton =
    row [ Background.color defaultButtonColor ]
        [ Input.button buttonStyle
            { onPress = Nothing
            , label = el [ centerX, centerY, Font.size 14 ] (text "Rendered text")
            }
        ]


rawModeButton : Mode -> Element Msg
rawModeButton currentMode =
    row [ Background.color (buttonColor currentMode RawHTMLMode) ]
        [ Input.button buttonStyle
            { onPress = Just (SetMode RawHTMLMode)
            , label = el [ centerX, centerY, Font.size 14 ] (text "Raw")
            }
        ]


renderedModeButton : Mode -> Element Msg
renderedModeButton currentMode =
    row [ Background.color (buttonColor currentMode RenderedMode) ]
        [ Input.button buttonStyle
            { onPress = Just (SetMode RenderedMode)
            , label = el [ centerX, centerY, Font.size 14 ] (text "Rendered")
            }
        ]



--
-- STYLE
--


mainColumnStyle model =
    [ centerX
    , centerY
    , bgGray 0.5
    , paddingXY 20 20
    , width (px (appWidth_ + 40))
    , height (px (appHeight_ model + 40))
    ]


buttonStyle =
    [ Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]


buttonStyle2 =
    [ Font.color (rgb255 255 255 255)
    , Background.color (rgb255 0 0 180)
    , paddingXY 15 8
    ]


grayColor g =
    Element.rgb g g g


whiteColor =
    grayColor 1
