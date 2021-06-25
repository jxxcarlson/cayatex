module Render.LaTeX exposing (render, renderAsDocument, renderImage)

import CYUtility
import Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Parser.Data
import Parser.Element as Element exposing (Element(..))
import Parser.Function
import Parser.Lines
import Parser.RunLoopFunctions
import Render.Elm
import Render.Types as Types
import Spreadsheet
import Widget.Data


renderAsDocument : String -> String
renderAsDocument sourceText =
    texPrefix ++ "\n\n" ++ render sourceText ++ texSuffix


render : String -> String
render sourceText =
    Parser.Lines.runLoop 0 (String.lines (transformText sourceText))
        |> Parser.Lines.toParsed
        |> transformAST
        |> renderList (initState 0)


transformText : String -> String
transformText str =
    str



--  String.replace "_" "\\_" str


transformAST : List (List Element) -> List (List Element)
transformAST parsand =
    List.map (List.map transformElement) parsand


transformElement : Element -> Element
transformElement element =
    case element of
        Element "sum" args body _ ->
            evalListFunction "sum" List.sum body

        Element "average" args body _ ->
            evalListFunction "average" Widget.Data.meanOfList body

        Element "stdev" args body _ ->
            evalListFunction "stdev" Widget.Data.stdevOfList body

        Element "spreadsheet" args body _ ->
            Text (spreadsheet body) Nothing

        _ ->
            element


spreadsheet : Element -> String
spreadsheet body =
    let
        spreadsheet1 =
            Render.Elm.getRows_ body |> List.Extra.transpose

        spreadsheet2 : List (List String)
        spreadsheet2 =
            Spreadsheet.evalText spreadsheet1
                |> List.Extra.transpose
    in
    tableOfListList spreadsheet2


tableOfListList : List (List String) -> String
tableOfListList data =
    let
        n =
            List.head data |> Maybe.map List.length |> Maybe.withDefault 0

        format =
            List.repeat n "r" |> String.join " " |> (\x -> "{ " ++ x ++ " }")

        table_ =
            List.map transformRow data |> String.join "\\\\\n"
    in
    "\\begin{indent}\n\\begin{tabular}" ++ format ++ "\n" ++ table_ ++ "\n" ++ "\\end{tabular}\n\\end{indent}"


transformRow : List String -> String
transformRow items =
    String.join " & " items


smudge : List (List String) -> String
smudge data =
    data |> List.map (String.join "\n") |> String.join " "


evalListFunction : String -> (List Float -> Float) -> Element -> Element
evalListFunction name f element =
    let
        argLists =
            argListsFromBody element

        val =
            argLists.floatArgs |> f |> CYUtility.roundTo 2
    in
    Text (name ++ " " ++ String.join ", " argLists.stringArgs ++ " = " ++ String.fromFloat val) Nothing


argListsFromBody : Element -> { stringArgs : List String, floatArgs : List Float }
argListsFromBody element =
    let
        stringArgs =
            stringListFromElement element

        floatArgs =
            listFloatOfListString stringArgs
    in
    { stringArgs = stringArgs, floatArgs = floatArgs }


stringListFromElement : Element -> List String
stringListFromElement element =
    element
        |> Parser.Function.getText
        |> Maybe.map CYUtility.commaSeparatedToList
        |> Maybe.withDefault []


listFloatOfListString : List String -> List Float
listFloatOfListString strings =
    strings
        |> List.map String.toFloat
        |> Maybe.Extra.values


type alias LaTeXDict =
    Dict String RenderingFunction


type alias RenderingFunction =
    Types.RenderArgs -> String -> List String -> String -> String



-- DICT


latexDict : LaTeXDict
latexDict =
    Dict.fromList
        [ aliasedSimpleEntry "bold" "textbf"
        , aliasedSimpleEntry "b" "textbf"
        , simpleEntry "italic"
        , aliasedSimpleEntry "i" "italic"
        , simpleEntry "strike"
        , simpleEntry "underline"
        , simpleEntry "red"
        , simpleEntry "blue"
        , simpleEntry "violet"
        , simpleEntry "highlight"
        , simpleEntry "white"
        , simpleEntry "medgray"
        , simpleEntry "black"
        , aliasedSimpleEntry "code" "violet"
        , aliasedSimpleEntry "c" "violet"
        , simpleEntry "section"
        , aliasedSimpleEntry "section1" "section"
        , aliasedSimpleEntry "section2" "subsection"
        , aliasedSimpleEntry "section3" "subsubsection"
        , aliasedSimpleEntry "section4" "subheading"
        , aliasedSimpleEntry "section5" "subheading"
        , aliasedSimpleEntry "section6" "subheading"
        , ( "title", \ra name args body -> title body )
        , ( "tableofcontents", \_ _ _ _ -> "\\tableofcontents" )
        , entry1 "link" "href"
        , ( "list", \ra name args body -> "\\begin{itemize}\n\n" ++ body ++ "\\end{itemize}" )
        , ( "item", \ra name args body -> "\\item " ++ body ++ "\n\n" )
        , ( "math", \ra name args body -> "$" ++ body ++ "$" )
        , ( "m", \ra name args body -> "$" ++ body ++ "$" )
        , ( "mathblock", \ra name args body -> "$$" ++ body ++ "$$" )
        , ( "mb", \ra name args body -> "$$" ++ body ++ "$$" )
        , ( "theorem", \ra name args body -> environment_ "theorem" body )
        , environmentEntry "theorem"
        , environmentEntry "corollary"
        , environmentEntry "indent"
        , aliasedEnvironmentEntry "codeblock" "verbatim"
        , aliasedEnvironmentEntry "cb" "verbatim"
        , entryForMacroWithArgs "fontRGB"
        , entryForMacroWithArgs "highlightRGB"
        , ( "image", \ra name args body -> renderImage args body )
        ]


fileReferenceFromUrl : String -> String
fileReferenceFromUrl url =
    case String.split "/" url |> List.reverse |> List.head of
        Nothing ->
            "no-file-name"

        Just fileName ->
            "image/" ++ fileName


renderImage : List String -> String -> String
renderImage args body =
    let
        dict =
            CYUtility.keyValueDict args

        caption =
            Dict.get "caption" dict |> Maybe.withDefault ""

        width =
            Dict.get "width" dict |> Maybe.andThen String.toInt |> Maybe.withDefault 100 |> String.fromInt

        placement =
            Dict.get "placement" dict |> Maybe.withDefault "center"

        args_ =
            argsToString [ fileReferenceFromUrl body, caption ]
    in
    "\\" ++ "imagecenter" ++ args_


argsToString : List String -> String
argsToString args =
    args
        |> List.map argToString
        |> String.join ""


argToString : String -> String
argToString arg =
    "{" ++ arg ++ "}"


fontRGB args body =
    "\\fontRGB" ++ argsToString args ++ argToString body


highlightRGB args body =
    "\\highlightRGB" ++ argsToString args ++ argToString body


entryForMacroWithArgs macroName =
    ( macroName, \ra name args body -> "\\" ++ macroName ++ argsToString args ++ argToString body )



-- entryForMacroWithArgs_ macroName


entryForMacroWithArgs_ macroName =
    \ra name args body -> "\\" ++ macroName ++ argsToString args ++ argToString body


environmentEntry envName =
    ( envName, \ra name args body -> environment_ envName body )


aliasedEnvironmentEntry alias envName =
    ( alias, \ra name args body -> environment_ envName body )


environment_ : String -> String -> String
environment_ envName body =
    "\\begin{" ++ envName ++ "}\n" ++ body ++ "\n\\end{" ++ envName ++ "}"


entry1 name macroname =
    ( name, \_ _ args body -> macro1 macroname args body )


macro1 name args body =
    let
        arg =
            List.Extra.getAt 0 args |> Maybe.withDefault body
    in
    "\\" ++ name ++ "{" ++ body ++ "}{" ++ arg ++ "}"


title : String -> String
title str =
    simpleMacro_ "title" str ++ "\n" ++ "\\maketitle"



-- SIMPLE ENTRY


aliasedSimpleEntry : String -> String -> ( String, RenderingFunction )
aliasedSimpleEntry alias name =
    ( alias, simpleMacro name )


simpleEntry : String -> ( String, RenderingFunction )
simpleEntry name =
    ( name, simpleMacro name )


simpleMacro : String -> RenderingFunction
simpleMacro name =
    \_ _ _ body -> simpleMacro_ name body


simpleMacro_ : String -> String -> String
simpleMacro_ name body =
    "\\" ++ name ++ "{" ++ body ++ "}"


renderDefaultElement : Types.RenderArgs -> String -> List String -> Element -> String
renderDefaultElement renderArgs name argList body =
    --"Element: " ++ name ++ " | " ++ String.join ", " argList ++ " | " ++ renderElement renderArgs body
    "\\violet{Not implemented: }" ++ "\\blue{" ++ name ++ "}"


renderList : Types.RenderArgs -> List (List Element) -> String
renderList renderArgs list_ =
    List.map (renderElements renderArgs) list_ |> String.join "\n\n"


renderElements : Types.RenderArgs -> List Element -> String
renderElements renderArgs list =
    List.map (renderElement renderArgs) list |> String.join ""


renderElement : Types.RenderArgs -> Element -> String
renderElement renderArgs element =
    case element of
        Text str _ ->
            str

        Element name argList body _ ->
            case Dict.get name latexDict of
                Nothing ->
                    renderDefaultElement renderArgs name argList body

                Just f ->
                    f renderArgs name argList (renderElement renderArgs body)

        LX elements _ ->
            List.map (renderElement renderArgs) elements |> String.join " "

        Problem p e _ ->
            "Error: " ++ e


initState : Int -> Types.RenderArgs
initState k =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = Parser.Data.init Parser.Data.defaultConfig
    }


texPrefix : String
texPrefix =
    """
\\documentclass[11pt, oneside]{article}

%% Packages
\\usepackage{geometry}
\\geometry{letterpaper}
\\usepackage{changepage}   % for the adjustwidth environment
\\usepackage{graphicx}
\\usepackage{wrapfig}
\\graphicspath{ {images/} }
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{amscd}
\\usepackage{hyperref}
\\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    filecolor=magenta,
    urlcolor=blue,
}
\\usepackage{xcolor}
\\usepackage{soul}


%% Commands
\\newcommand{\\code}[1]{{\\tt #1}}
\\newcommand{\\ellie}[1]{\\href{#1}{Link to Ellie}}
% \\newcommand{\\image}[3]{\\includegraphics[width=3cm]{#1}}

\\newcommand{\\imagecenter}[3]{{
   \\medskip
   \\begin{figure}
   \\centering
    \\includegraphics[width=12cm,height=12cm,keepaspectratio]{#1}
    \\vglue0pt \\par {#2}
    \\end{figure}
    \\medskip
}}

\\newcommand{\\imagefloatright}[3]{
    \\begin{wrapfigure}{R}{0.30\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}

\\newcommand{\\imagefloatleft}[3]{
    \\begin{wrapfigure}{L}{0.3-\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}

\\newcommand{\\italic}[1]{{\\sl #1}}
\\newcommand{\\strong}[1]{{\\bf #1}}
\\newcommand{\\subheading}[1]{{\\bf #1}\\par}
\\newcommand{\\xlink}[2]{\\href{{https://minilatex.lamdera.app/g/#1}}{#2}}
\\newcommand{\\red}[1]{\\textcolor{red}{#1}}
\\newcommand{\\blue}[1]{\\textcolor{blue}{#1}}
\\newcommand{\\violet}[1]{\\textcolor{violet}{#1}}
\\newcommand{\\remote}[1]{\\textcolor{red}{#1}}
\\newcommand{\\local}[1]{\\textcolor{blue}{#1}}
\\newcommand{\\highlight}[1]{\\hl{#1}}
\\newcommand{\\note}[2]{\\textcolor{blue}{#1}{\\hl{#1}}}
\\newcommand{\\strike}[1]{\\st{#1}}
\\newcommand{\\term}[1]{{\\sl #1}}
\\newtheorem{remark}{Remark}
\\newcommand{\\comment}[1]{}
\\newcommand{\\innertableofcontents}{}

%% Theorems
\\newtheorem{theorem}{Theorem}
\\newtheorem{axiom}{Axiom}
\\newtheorem{lemma}{Lemma}
\\newtheorem{proposition}{Proposition}
\\newtheorem{corollary}{Corollary}
\\newtheorem{definition}{Definition}
\\newtheorem{example}{Example}
\\newtheorem{exercise}{Exercise}
\\newtheorem{problem}{Problem}
\\newtheorem{exercises}{Exercises}
\\newcommand{\\bs}[1]{$\\backslash$#1}
\\newcommand{\\texarg}[1]{\\{#1\\}}

%% Environments
\\renewenvironment{quotation}
  {\\begin{adjustwidth}{2cm}{} \\footnotesize}
  {\\end{adjustwidth}}

\\def\\changemargin#1#2{\\list{}{\\rightmargin#2\\leftmargin#1}\\item[]}
\\let\\endchangemargin=\\endlist

\\renewenvironment{indent}
  {\\begin{adjustwidth}{0.75cm}{}}
  {\\end{adjustwidth}}


\\definecolor{mypink1}{rgb}{0.858, 0.188, 0.478}
\\definecolor{mypink2}{RGB}{219, 48, 122}

\\newcommand{\\fontRGB}[4]{
    \\definecolor{mycolor}{RGB}{#1, #2, #3}
    \\textcolor{mycolor}{#4}
    }

\\newcommand{\\highlightRGB}[4]{
    \\definecolor{mycolor}{RGB}{#1, #2, #3}
    \\sethlcolor{mycolor}
    \\hl{#4}
     \\sethlcolor{yellow}
    }

\\newcommand{\\gray}[2]{
\\definecolor{mygray}{gray}{#1}
\\textcolor{mygray}{#2}
}

\\newcommand{\\white}[1]{\\gray{1}[#1]}
\\newcommand{\\medgray}[1]{\\gray{0.5}[#1]}
\\newcommand{\\black}[1]{\\gray{0}[#1]}

% Spacing
\\parindent0pt
\\parskip5pt

\\begin{document}


"""


{-| Boilerplate for the end of the document.
-}
texSuffix : String
texSuffix =
    """

\\end{document}
"""



-- \\newcommand{\\bibhref}[3]{[#3]\ \\href{#1}{#2}}
