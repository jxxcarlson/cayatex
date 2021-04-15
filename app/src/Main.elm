module Main exposing (keyedNode, main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
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
import Parser.Document
import Parser.Element as Parser
import Parser.Getters
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
    }


type Msg
    = NoOp
    | InputText String
    | SetMode Mode
    | Mark2Msg Render.Elm.Mark2Msg


type Mode
    = RawHTMLMode
    | RenderedMode


type alias Flags =
    {}


initialText =
    Data.text


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = initialText
      , renderedText = Render.String.renderString initialText
      , mode = RenderedMode
      , count = 0
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

        Mark2Msg _ ->
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


widePanelWidth =
    px (2 * panelWidth_ + 12)


panelWidth_ =
    400


panelWidth =
    px panelWidth_


panelHeight =
    px 370


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 36, width (px 900), height (px 900) ]
            [ title "CaYaTeX"
            , column [ spacing 12 ]
                [ row [ spacing 12 ] [ inputText model, outputDisplay model ]
                , parserDisplay model
                ]
            , row [ Font.size 14, Font.color whiteColor ] []
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]


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
            el [ alignTop ] (column [ width widePanelWidth, height (px 200), scrollbarY ] (List.map (\s -> Element.paragraph [] [ text s ]) (parsed_ pt)))


parsed_ : a -> List String
parsed_ pt =
    Paragraph.lines paragraphFormat2 (Debug.toString pt)


outputDisplay : Model -> Element Msg
outputDisplay model =
    column [ spacing 8 ]
        [ row
            [ fontGray 0.9
            , spacing 12
            , moveUp 9
            , Font.size 14
            ]
            [ rawModeButton model.mode, renderedModeButton model.mode, text ("Count: " ++ String.fromInt model.count) ]
        , outputDisplay_ model
        ]


outputDisplay_ : Model -> Element Msg
outputDisplay_ model =
    column
        [ spacing 8
        , Background.color (Element.rgb 1.0 1.0 1.0)
        , paddingXY 8 12
        , width panelWidth
        , height panelHeight
        , scrollbarY
        , moveUp 9
        , Font.size 12
        ]
        (if model.mode == RenderedMode then
            -- render model.count model.renderedText
            [ render2 model.count model.input ]

         else
            List.map text (Paragraph.lines paragraphFormat model.renderedText)
        )



-- TODO: Working on this now


render2a : Int -> String -> Element Msg
render2a k str =
    Render.Elm.renderString k 0 str |> Element.map Mark2Msg



-- render2x : List String -> List (List Parser.Element)
-- render2x : List String -> List (List (Element Render.Elm.Mark2Msg))


render2 : Int -> String -> Element Msg
render2 k str =
    Parser.Document.runProcess k (String.lines str)
        |> Parser.Document.toParsed
        |> List.reverse
        |> List.map (Render.Elm.renderList k 0)
        |> column [ spacing 18 ]
        |> Element.map Mark2Msg



--renderList : Int -> Int -> List Parser.Element.Element -> Element Mark2Msg
--renderList generation blockOffset list
-- |> List.map (List.map Render.Elm.renderElementX)
-- |> List.map (Render.Elm.renderElement)


paragraphFormat =
    { maximumWidth = 80, optimalWidth = 70, stringWidth = String.length }


paragraphFormat2 =
    { maximumWidth = 160, optimalWidth = 150, stringWidth = String.length }


render : Int -> String -> List (Element msg)
render k data =
    case Html.Parser.run data of
        Err _ ->
            [ text "Error converting to HTML" ]

        Ok element ->
            [ keyedNode k element ]


keyedNode : Int -> List Html.Parser.Node -> Element msg
keyedNode k element =
    Html.Parser.Util.toVirtualDom element
        |> keyIt k
        |> Html.Keyed.node "div" []
        |> Element.html


keyIt : Int -> List b -> List ( String, b )
keyIt k list =
    List.indexedMap (\i e -> ( String.fromInt (i + k), e )) list


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ height panelHeight, width panelWidth, Font.size 14 ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Source text")
        , spellcheck = False
        }



-- buttonColor : Mode -> Mode ->


buttonColor buttonMode currentMode =
    if buttonMode == currentMode then
        Element.rgb255 130 12 9

    else
        Element.rgb255 60 60 60


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


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 0.5
    , paddingXY 20 20
    ]


buttonStyle =
    [ Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]


grayColor g =
    Element.rgb g g g


whiteColor =
    grayColor 1
