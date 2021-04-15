module Data exposing (text)


tt =
    "test"


text =
    """Pythagoras says that [math a^2 + b^2 = c^2].  
This is an [strong [italic extremely]] cool result. But just as cool is the below:
[mathDisplay \\sum_1^\\infty 1/n = \\infty,]
which goes back to the work of Nicole Oresme
 (1320–1382).  See the entry in the [link |Stanford Encyclopedia of Philosophy| https://plato.stanford.edu/entries/nicole-oresme/].

[theorem There are infinitely many primes [math p \\equiv 1 (4).]]

[corollary |Euclid| There are infinitely many primes.]

Some code: [code col :: Int -> Matrix a -> \\[a\\]]. 
Do you recognize the language (ha ha)?
[italic [highlight And can we do something about the awkwardness of escaping brackets inside code elements?]]

Example:  [highlightRGB |214, 93, 32| [fontRGB |169, 232, 245| What color is this?]]

[strong Note:] We need to implement a macro facility so that users can abbreviate constructs like
the one in the previous example.


"""


yyy =
    """
[strong Errors:] If a user mistypes an element name or tries to use one that is unimplemented, here
is what happens: [foo this is a test].
"""
