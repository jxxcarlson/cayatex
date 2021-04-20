module Data exposing (test, text)


test =
    """
 [strong Chart]
 
 [bargraph 0, 1, 2, 4,  3,  2, 1]
 
 This is a chart
"""


test2 =
    """A
           
[strong C]
D [strong E]"""


test3 =
    """[strong A]
[strong B]
C

[strong A]
[strong B]
[strong D]

[strong A]
[strong B]
C [strong D]"""


text =
    """
[section CaYaTeX Test Document]

By James Carlson and Nicholas Yang

%Notice the a leading percent sign makes a line into a comment.

[italic CaYaTeX is a simple yet powerful markup language that
compiles to both LaTeX and Html.]

[italic The present document, written entirely in CaYaTeX,
lays out our design goals and demonstrates some of the
progress we have made towards specifiying the language and implementing it in Elm. While our work
is incomplete, it
is a start.]

[italic We are also working on an implementation in Rust. This will,
among other things, help us to cross-validate the specification.]

The project is open source: [link https://github.com/jxxcarlson/cayatex]


[subsection Design Goals]

The goals of the CaYaTeX project are for the language to be

[list |numbered|

[item [strong Small], hence easy to learn. [italic To this end there are just two constructs: ordinary text and [code elements]].]

[item [strong Powerful].  We borrow ideas from functional programming.
Elements have a Lisp-like syntax with brackets in place of parentheses.
An element has the form [code raw##[name |argument-list| body]##] or simply  [code raw##[name body]##]
The argument list is a comma-delimited sequence of
strings.  The body is an element.
The partial element [code name args] is a function [code Element -> Element].
Such functions can be composed, as in mathematics or as in languages such as Haskell and Elm.
]


[item [strong Extensible]. [italic Via a macro facility].]

[item [strong Multiple inputs and outputs.] Documents written in CaYaTeX can be compiled to LaTeX, Markdown, and HTML. Markdown documents can be compiled to CaYaTeX.]

[item [strong Web-ready]. CaYaTeX has a differential compiler that makes it suitable for real-time editing, e.g.,  in a web app. ]

[item [strong Kind and Helpful]. Displays friendly and informative error messages in real time in the rendered text; has hooks for highlighting the corresponding source text in a suitable IDE/editor.]

[item [strong Modern]. Unicode compatible.]]

[strong Note.] [fontRGB |50, 0, 200| At the moment  we have not yet implemented differential compilation, which greatly
speeds up compilation during editing.  All in due time!]

[subsection Mathematics]


Pythagoras says that [math a^2 + b^2 = c^2].
This is an [strong [italic extremely]] cool result. But just as cool is the below:

[mathdisplay \\sum_{n=1}^\\infty \\frac{1}{n} = \\infty,]

which goes back to the work of Nicole Oresme (1320–1382).  See the entry in the
[link |Stanford Encyclopedia of Philosophy| https://plato.stanford.edu/entries/nicole-oresme/].
You can also consult [link https://en.wikipedia.org/wiki/Nicole_Oresme].

We can also do some high-school math, with that beautifully curved integral sign
that attracts so many people to the subject:

[mathdisplay \\int_0^1 x^n dx = \\frac{1}{n+1}]

And of course, we can also do theorems:

[theorem There are infinitely many primes [math p \\equiv 1 \\text{ mod } 4.]]

[corollary |Euclid| There are infinitely many primes.]

[subsection Data]

One can design elements which manipulate data (numerical computations, visualization).  Here are some data computations:

[sum 1.2, 2, 3.4, 4]

[average 1.2, 2, 3.4, 4]

[stdev |precision:3| 1.2, 2, 3.4, 4]

In the numerical examples, the precision of the result has a default value of 2.  This can be changed, as one sees in the source of the third example, e.g., you can have

[codeblock raw##[stdev | 1.2, 2, 3.4, 4]## ]

or

[codeblock raw##[stdev |precision:3| 1.2, 2, 3.4, 4]## ]


[subsection Graphs]

Below are two simple data visualizations. We plan more, and more configurability of what you see here.

[bargraph 1, 1.2, 3,  1, 2, 4,  3,  2, 1, 2, 3, 5, 3, 7, 3, 2, 9, 8, 7, 0, 0]

The bargraph code:

[codeblock raw##[bargraph 1, 1.2, 3, 1, ... ]## ]

[linegraph 0, 0
1, 10
2, 5
3, 3
4, 9
5, 11
6, 4
7, 6
8, 10
9, 1
]

The linegraph code (CSV format):

[codeblock raw##[linegraph
0, 0
1, 10
...
8, 10
9, 1
]
##]

[subsection Unicode]

You can freely use unicode characters, as in this poetry element:

[poetry
А я иду, где ничего не надо,
Где самый милый спутник — только тень,
И веет ветер из глухого сада,
А под ногой могильная ступень.

— Анна Ахматова
]

[subsection Code]

Time for some code: [code raw##col :: Int -> Matrix a -> [a]##].
Do you recognize the language (ha ha)?

We can also do code blocks.  Syntax highlighting coming later.

[codeblock raw##
# For Sudoku 3x3 subsquare function

col :: Int -> Matrix a -> [a]
col k = fmap ( !! k)

cols :: Matrix a -> Matrix a
cols m =
    fmap (\\k -> col k m) [0..n]
       where n = length m - 1
##]


[italic [highlight Note the use of Rust-like raw strings in the source text to avoid escaping all the brackets.]]



[subsection Color]

Example:  [highlightRGB |214, 93, 32| [fontRGB |169, 232, 245| What color is this?]]

[strong Note:] We need to implement a macro facility so that users can abbreviate constructs like
the one in the previous example.

[subsection Images]

[image |caption: Rotkehlchen aufgeplustert, width: 200, placement: center|https://i.pinimg.com/originals/d4/07/a4/d407a45bcf3ade18468ac7ba633244b9.jpg]

[subsection Lists]

Note that lists can be nested and can be given a title if desired.  The symbol for "bulleted" lists is • by default, but can be specified by the user.
A numberd list has "numbered" as its first argument, as in the example below.

[list |numbered, title:Errands and other stuff|

    [item Bread, milk, O-juice]

    [item Sand paper, white paint]

    [list |none|

        [item A]

        [item B]

        [list |§, title:Greek symbols|

            [item [math \\alpha = 0.123]]

            [item  [math \\beta = 4.567]]

]]]
"""


stuff =
    """% Notice the a leading percent sign makes a line into a comment.           
%
%[subsection About Errors]
%
%Look what happens here:
%
%(1) [ital One more beer, please!]
%
%(2) [italic One more beer, please
           """
