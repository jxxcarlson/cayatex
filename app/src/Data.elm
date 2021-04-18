module Data exposing (test, text)


test =
    """before

[subsection A

[strong B]
[strong C]
[strong D]
[strong E]

after
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

[fontRGB |12, 140, 176| CaYaTeX is a simple yet powerful markup language that
compiles to both LaTeX and Html.]

[fontRGB |12, 140, 176| The present document, written in CaYaTeX,
lays out our design goals and demonstrates some of the
progress we have made towards specifiying the language and implementing it in Elm. While our work
is incomplete, it
is a start.]

% [fontRGB |12, 140, 176| We are also working on implementations in Rust. This will,
% among other things, help us to cross-validate both implementations of the
% specification.]


[subsection Design Goals]

The goals of the CaYaTeX project are for the language to be

[list

[item [strong Small], hence easy to learn. [italic To this end there are just two constructs: ordinary text and [code elements]].]

[item [strong Powerful].  [italic To this end, we borrow ideas from functional programming.  Elements have a Lisp-like syntax. An element has a name, an argument list, and a body (which is also an argument). The partial element [code name args] is a function [code Element -> Element]. Such functions can be composed, as in mathematics or as in languages such as Haskell and Elm.]]

[item [strong Extensible]. [italic Via a macro facility].]

[item [strong Multiple inputs and outputs.] Documents written in CaYaTeX can be compiled to LaTeX, Markdown, and HTML. Markdown documents can be compiled to CaYaTeX.]

[item [strong Web-ready]. CaYaTeX has a differential compiler that makes it suitable for real-time editing, e.g.,  in a web app. ]

[item [strong Kind and Helpful]. Displays friendly and informative error messages in real time in the rendered text; has hooks for highlighting the corresponding source text in a suitable IDE/editor.]

[item [strong Modern]. Unicode compatible.]]


[subsection Mathematics]


Pythagoras says that [math a^2 + b^2 = c^2].
This is an [strong [italic extremely]] cool result. But just as cool is the below:
[mathDisplay \\sum_1^\\infty 1/n = \\infty,]
which goes back to the work of Nicole Oresme (1320–1382).  See the entry in the
[link |Stanford Encyclopedia of Philosophy| https://plato.stanford.edu/entries/nicole-oresme/].
You can also consult [link https://en.wikipedia.org/wiki/Nicole_Oresme].

[theorem There are infinitely many primes [math p \\equiv 1 (4).]]

[corollary |Euclid| There are infinitely many primes.]


[subsection Code]

Time for some code: [code col :: Int -> Matrix a -> \\[a\\]].
Do you recognize the language (ha ha)?
[italic [highlight And can we do something about the awkwardness of escaping brackets inside code elements?]]

[subsection Color]

Example:  [highlightRGB |214, 93, 32| [fontRGB |169, 232, 245| What color is this?]]

[strong Note:] We need to implement a macro facility so that users can abbreviate constructs like
the one in the previous example.

[subsection Images]

[image |caption: Rotkehlchen aufgeplustert, width: 200, placement: center|https://i.pinimg.com/originals/d4/07/a4/d407a45bcf3ade18468ac7ba633244b9.jpg]

[subsection Lists]

Note that lists can be nested and can be given a title if desired.  The symbol for "bulleted" lists is • by default, but can be specified by the user.  We have not yet implemented numbered lists.

[list |title:Errands and other stuff|

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
