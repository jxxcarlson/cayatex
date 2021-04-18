module Data exposing (test, text)


test =
    """[list |bullet|

[item 1]

[item 2]

[list [item A] [item B]]

]


"""


text =
    """[section CaYaTeX Test Document]

Pythagoras says that [math a^2 + b^2 = c^2].
This is an [strong [italic extremely]] cool result. But just as cool is the below:
[mathDisplay \\sum_1^\\infty 1/n = \\infty,]
which goes back to the work of Nicole Oresme (1320–1382).  See the entry in the
[link |Stanford Encyclopedia of Philosophy| https://plato.stanford.edu/entries/nicole-oresme/].
You can also consult [link https://en.wikipedia.org/wiki/Nicole_Oresme].

[theorem There are infinitely many primes [math p \\equiv 1 (4).]]

[corollary |Euclid| There are infinitely many primes.]

Time for some code: [code col :: Int -> Matrix a -> \\[a\\]].
Do you recognize the language (ha ha)?
[italic [highlight And can we do something about the awkwardness of escaping brackets inside code elements?]]

Example:  [highlightRGB |214, 93, 32| [fontRGB |169, 232, 245| What color is this?]]

[strong Note:] We need to implement a macro facility so that users can abbreviate constructs like
the one in the previous example.

[image |caption: Rotkehlchen aufgeplustert, width: 200, placement: center|https://i.pinimg.com/originals/d4/07/a4/d407a45bcf3ade18468ac7ba633244b9.jpg]

[subsection Lists, etc.]

[strong Errands]

[list |bullet|

  [item Bread, milk, O-juice]

  [item Sand paper, white paint]

]

[subsection About Errors]

Look what happens here:

(1) [ital One more beer, please!]

(2) [italic One more beer, please
"""
