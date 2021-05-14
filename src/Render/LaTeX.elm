module Render.LaTeX exposing (render, renderAsDocument)

import Dict exposing (Dict)
import List.Extra
import Parser.Data
import Parser.Document
import Parser.Element exposing (Element(..))
import Render.Types as Types


renderAsDocument : String -> String
renderAsDocument sourceText =
    texPrefix ++ "\n\n" ++ render sourceText ++ texSuffix


render : String -> String
render sourceText =
    Parser.Document.runLoop 0 (String.lines sourceText)
        |> Parser.Document.toParsed
        |> renderList (initState 0)


type alias LaTeXDict =
    Dict String RenderingFunction


type alias RenderingFunction =
    Types.RenderArgs -> String -> List String -> String -> String



-- DICT


latexDict : LaTeXDict
latexDict =
    Dict.fromList
        [ simpleEntry "bold"
        , aliasedSimpleEntry "b" "bold"
        , simpleEntry "italic"
        , aliasedSimpleEntry "i" "italic"
        , simpleEntry "strike"
        , simpleEntry "underline"
        , simpleEntry "red"
        , simpleEntry "blue"
        , simpleEntry "highlight"
        , simpleEntry "code"
        , aliasedSimpleEntry "c" "code"
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
        ]


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
    "Element: " ++ name ++ " | " ++ String.join ", " argList ++ " | " ++ renderElement renderArgs body


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
