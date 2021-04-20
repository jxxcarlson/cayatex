# CaYaTeX: an Experimental Markup Language (Draft!)

by James Carlson and Nicholas Yang


CaYaTeX is an experimental markup language that compiles to LaTeX, Html, ahd (indirectly) PDF. 

There are two constructs in the language: _text_ and _elements_. Below are
some elements.  They have the form `[name |args| body]` where (a) `|args|` may be
absent, (b) `|args|` is present, it has the form `|arg1, arg2, ...|` where the 
individual args are strings without commas, (c) the body is an Element.

1. [strong whiskey and other liquor]
2. [italic [strong stuff]]  
3. [math a^2 + b^2 = c^2]  
4. [image |width:500, height:300, align:center| https://pythagoras.io/proof.png]
5. [theorem |Pythagoras| For any right triangle, [math a^2 + b^2 = c^2], where
   [math a], [math b] are the legs, and [math c] is the hypotenuse.]
   
**Note.** See branch *master* for a variant of the language which contains *Blocks.*   

*Comments:*

1. The function `strong` takes a single argument, the body, which
is the string "whiskey and other liquor".

2. Elements can be nested.

3. Example three uses TeX/LaTeX-style mathematical notation.

4. In example four, the element takes to arguments in addition to its body.

5. This element maps to a LaTeX theorem environment.

One of the design goals is to have a clean, simple, and uniform syntax with as few
constructs as possible.  We aim to show that simple can be powerful, easy to learn,
and easy to use.

### Another examples

Here is a piece of text that parses to an inline expression:

	Pythagoras, a [italic [strong really, really] good] mathematician, showed
	that the sides of a right triangle satisfy the relation
	[math a^2 + b^2 = c^2].  For more information, see
	[link |Wikipedia| https://en.wikipedia.org/wiki/Pythagorean_theorem].
	For the idea of the proof, see
	[image |width:500, height:300, align:center| https://pythagoras.io/proof.png]


### Grammar

The grammar for elements is as follows.

type Element
= Text String (Maybe SourceMap)
| Element String (List String) Element (Maybe SourceMap)
| LX (List Element) (Maybe SourceMap)



	Element ->   
         Text String
         | "[" Identifier  Element "]"
         | "[" Identifier "|" Args "|"  Element "]"
	Args ->
         NonemptyString ("," NonemptyString)* 



### More Examples

Here is how one writes a list:

	[numbered-list 

    [item Raspberry jam]
    
    [item Sourdough bread]
    
    ]

A table is written like this:

   [table
   
   [row Hydrogen, 1, 1]
   
   [row Helium, 2, 4]
   
   [row Lithium, 3, 6]
   
   ]

One can have a header row and a row that
defines the format.

One also has CSV tables:

   [csv
   Hydrogen, 1, 1
   Helium, 2, 4
   Lithium, 3, 6
   ]

as well as verbatim text:

   [verbatim
   This is a test:
   indented 2
   
       indented 4
   ]

## Shorthand

For a few common constructs, there are also shorthand features
a la markdown. The idea is to make the composition of text more ergonomic.
Thus, one can also say

	[numbered-list

	- eggs

	- milk

	- bread

	]

For italic text, one can say _italic text_, and for bold text, one can say *bold text*.
For sections, etc., one says

   [section Introduction]
   [subsection Examples]

NOTE: What about saying

   [section |1| Introduction]
   [section |2| Examples]

where the first argument gives the _level_ of the section.
Pro: this makes it easier to shift levels programmatically.
Con: noiser syntax.

In shorthand, this becomes

	# Introduction
	## Exmaples

We intend the use of shorthand to be quite limited, perhaps just to the above examples.

**Question.** Should the shorthand be handled by preprocessor that maps the source text
to standard, no-shorthand CaYaTeX?  What impact would this have on performance in an
interactive editing scenario?

## Parser Architecture

### Types

The type of the AST is as below.

```elm
type Element
    = Text String (Maybe SourceMap)
    | Element String (List String) Element (Maybe SourceMap)
    | LX (List Element) (Maybe SourceMap)
```

The SourceMap locates the parsed expression in the source text.
We assume that the text is a list of strings that defines
a list of text blocks.  A text block is either
an ordinary paragraph (non-blank lines bordered by blank lines)
or an outer block.  Here is an example:

	0 This is a test.
	1 Ho ho ho!
	2
	3 {theorem|There are [strong many] primes!}
	4
	5 One, two, [italic three!]

In this example there are three blocks, with blockOffsets
0, 3, and 7.  The source map for [strong many] is

	blockOffset = 3
	offset = 19
	length = 13

To arrive at this conclusion, convert the list of
lines 3, 4, 5 to the string

	blockText = "{theorem|There are [strong many] primes!}"

and find the offset of the substring "[strong many]".
Consequently we have

	String.slice 19 (19 + 13) blockText == "[strong many]"

The generation is supplied to parseLoop as an argument
with the assumption that it is incremented for each
edit.


```elm
type alias SourceMap =
	{ blockOffset : Int
	, offset : Int
	, length : Int
	, generation : Int
	}
```

### Functions

((To be expanded))

Top level function: Parser.Driver.parseLoop.  This function repeatedly
calls Parser.Expression.parser until input is exhausted and a final
TextCursor is computed.  The role of the TextCursor will be explained
later.

## Questions and Issues

## On the question of one construct or two

Re the idea of using only one construct — e.g. inline elements — it
works.  All of the examples below are parsed into the current AST type.
If we adopt this idea, we should change the name of InLine to something
else.

Re the type of arguments, in the current setup, an Inline element
has a (function) name, a list of arguments, and a body.  The list
of arguments is a possibly empty list of strings.  The body
is a possibly empty list of InlineExpressions.  An InlineExpression
is either Text or an Inline.  Thus the body is parsed but the args
are not.

Note that this setup (see example below) can handle verbatim text
without having some special treatment by the parser.

There is a theoretical reason why we don't need two constructs: if we
can define functions, we can do anything.  The only downside that I see
is that LaTeX has text, macros, and environments, so the mapping 
from CaYaTeX to LaTeX will not be quite so straightforward.  We will
probably need a dictionary which tells us which function names map
to macros and which map to environments.  For rendering to Html, we 
won't need this.


## Prior Art

[Pollen](https://docs.racket-lang.org/pollen/)

[LaTeX](https://www.latex-project.org/) (of course)

[SwiftLaTeX](https://github.com/SwiftLaTeX/SwiftLaTeX)

[Tectonic](https://tectonic-typesetting.github.io/en-US/)
