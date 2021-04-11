# Mark2: an Experimental Markup Language (Draft!)

by James Carlson and Nicholas Yang


Mark2 is an experimental markup language that compiles to LaTeX and to Html.   There are two constructs in the language: _inline expressions_ and _blocks_.
The former are like LaTeX macros and the latter are like LaTeX environments.

One of the design goals is to have a clean, simple, and uniform syntax with as few
constructs as possible.  We aim to show that simple can be powerful.

## Inline Expressions

Here is a piece of text that parses to an inline expression:

	Pythagoras, a [italic [bold really, really] good] mathematician, showed
	that the sides of a right triangle satisfy the relation
	[math a^2 + b^2 = c^2].  For more information, see
	[link |Wikipedia| https://en.wikipedia.org/wiki/Pythagorean_theorem].
	For the idea of the proof, see
	[image |width:500, height:300, align:center| https://pythogoras.io/proof.png]


### Grammar

The grammar of inline expressions is as follows.

	InlineExpression ->   
         Text String
         | Inline "[" Name Args Body "]"
         | List InlineExpression
	Args -> 
         Empty 
         | "|" NonemptyString ("," NonemptyString)* "|"
	Body -> InlineExpression


### The idea

The idea behind both inlines and blocks is that they are functions. In the
above example, _bold_ is a function whose argument is the string "really, really"
 and _italic_ is function whose argument is [bold really, really]. In the
 expression italic [bold really, really] good], the functions _italic_ and
 _bold_ are composed.

The arguments to a function take form Args Body.  In [bold icky stuff],
Args is empty and Body = "icky stuff".  In

	[image |width:400, height:250| https://yada.io/xy.jpg],

Args = |width:400, height:250| and Body = https://yada.io/xy.jpg.


### Examples

```
> run (parser 1 2) "This is a test." |> Result.map strip
  Ok (Text ("This is a test.") Nothing)

> run (parser 1 2) "[strong  stuff]" |> Result.map strip
  Ok (Inline "strong" [] (Text (" stuff") Nothing) Nothing)

> run (parser 1 2) "[image |height:40,width:100| stuff]" |> Result.map strip
  Ok (Inline "image" ["height:40","width:100"] (Text "stuff" Nothing) Nothing)
```

## Blocks

Here is a piece of text that parses to a block:

	{theorem|There are infinitely many primes [math p \equiv 1 \modulo 4].}

The body of the block, the line "There are ..." is an inline expression.
The body can also be a block, as in the case of the nested blocks below.
The inner block has argument.

	{indent|
    {theorem [Pythagoras]|There are infinitely many primes [math p \equiv 1 \modulo 4].}
	}


### Grammar

The grammar for blocks is as follows.

	Block -> "{" Name "|" Args "}"
			  | "{" Name "|" Expression "}"
			  | "{" Name Args "|" Expression "}"
	Name -> String
    BlockArgs -> 
         Empty 
         | NonemptyString ("," NonemptyString)* 

### Examples

Here is how one writes a list:

	{numbered-list|

	{item| Eggs}

	{item| Milk}

	{item| Bread}

	}

A table is written like this:

    {table|
    {row| Hydrogen, 1, 1}
    {row| Helium, 2, 4}
    {row| Lithium, 3, 6}
    }

Or like this:

    {table|
    {format| l, c, r, r}
    {row| Hydrogen, 1, 1}
    {row| Helium, 2, 4}
    {row| Lithium, 3, 6}
    }

One also has CSV tables:

    {csv|
    Hydrogen, 1, 1
    Helium, 2, 4
    Lithium, 3, 6
    }


## Shorthand

For common constructs, there are also shorthand features
a la markdown. The idea is to make the composition of text more ergonomic.
Thus, one can also say

	|numbered-list|

	- eggs

	- milk

	- bread

	|end

For italic text, one can say _italic text_, and for bold text, one can say *bold text*.
For sections, etc., one says

|section Introduction
|subsection Examples

NOTE: What about saying

|section [1] Introduction
|section [2] Examples

where the first argument gives the _level_ of the section.
Pro: this makes it easier to shift levels programmatically.
Con: noiser syntax.

In shorthand, this becomes

	# Introduction
	## Exmaples

We intend the use of shorthand to be quite limited, perhaps just to the above examples.

## Parser Architecture

### Types

The type of the AST is as below.

```elm
type Expression
	= Text String (Maybe SourceMap)
	| Inline String (List String) Expression (Maybe SourceMap)
	| Block String (List String) (Maybe Expression) (Maybe SourceMap)
	| List Expression (Maybe SourceMap)
```

The SourceMap locates the parsed expression in the source text.
We assume that the text is a list of strings that defines
a list of text blocks (not Blocks!).  A text block is either
an ordinary paragraph (non-blank lines bordered by blank lines)
or an outer Block.  Here is an example:

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


## Prior Art

[Pollen](https://docs.racket-lang.org/pollen/)

[LaTeX](https://www.latex-project.org/) (of course)

[SwiftLaTeX](https://github.com/SwiftLaTeX/SwiftLaTeX)

[Tectonic](https://tectonic-typesetting.github.io/en-US/)
