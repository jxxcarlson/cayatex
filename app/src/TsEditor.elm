module TsEditor exposing (view)

-- import View.CodeExample exposing (codeTabs, iconArea)

import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Html.Styled.Events
import Html.Styled.Keyed
import Json.Encode
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import ZipList exposing (ZipList)


view : String -> Html Int
view exampleTs =
    div
        [ css
            [ Tw.rounded_xl
            , Tw.shadow_2xl
            , Tw.bg_black
            , Tw.rounded_lg
            , Tw.shadow_lg
            ]
        ]
        [ --iconArea
          --, codeTabs (ZipList.singleton ( "index.ts", "" ))
          editorNode exampleTs
        ]


editorNode : String -> Html msg
editorNode exampleTs =
    Html.Styled.Keyed.node "div"
        []
        [ ( "ts-editor-1"
          , node "ts-editor"
                [ Attr.property "editorValue" <| Json.Encode.string exampleTs
                , Attr.property "dts" <| Json.Encode.string """export type JsonObject = { [Key in string]?: JsonValue };


export interface JsonArray extends Array<JsonValue> {}

/**
Matches any valid JSON value.
Source: https://github.com/sindresorhus/type-fest/blob/master/source/basic.d.ts
*/
export type JsonValue =
 | string
 | number
 | boolean
 | null
 | JsonObject
 | JsonArray;

export interface ElmApp {
 ports: {
   interopFromElm: PortFromElm<FromElm>;
   interopToElm: PortToElm<ToElm>;
   [key: string]: UnknownPort;
 };
}

export type FromElm = { tag : "logEvent"; data : { severity : "info" | "warning" | "error"; message : string } };

export type ToElm = { data : { last : string; first : string }; tag : "gotUserInfo" };

export type Flags = { os : "Windows" | "MacOS" | "Linux" | "Unknown" };

export namespace Main {
 function init(options: { node?: HTMLElement | null; flags: Flags }): ElmApp;
}

export as namespace Elm;

export { Elm };

export type UnknownPort = PortFromElm<unknown> | PortToElm<unknown> | undefined;

export type PortFromElm<Data> = {
 subscribe(callback: (fromElm: Data) => void): void;
 unsubscribe(callback: (fromElm: Data) => void): void;
};

export type PortToElm<Data> = { send(data: Data): void };
"""
                , css
                    [ Tw.max_w_sm
                    , Tw.h_28
                    ]
                ]
                []
          )
        ]
