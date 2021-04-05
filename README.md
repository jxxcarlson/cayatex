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
    [link [Wikipedia] https://en.wikipedia.org/wiki/Pythagorean_theorem].


### Grammar

The grammar of inline expressions is as follows.  

    InlineExpression -> Text String | Inline "[" Name Args Body "]" | List InlineExpression
    Args -> Empty | "[" NonemptyString ("," NonemptyString)* "]" 
    Body -> String
    Word -> String with no spaces

### The idea

The idea behind both inlines and blocks is that they are functions. In the 
above example, _bold_ is a function whose argument is the string "really, really"
 and _italic_ is function whose argument is [bold really, really]. In the
 expression italic [bold really, really] good], the functions _italic_ and
 _bold_ are composed.

The arguments to a function take form Args Body.  In [bold icky stuff], 
Args is empty and Body = "icky stuff".  In 

    [image width:400, height:250| https://yada.io/xy.jpg],

Args = [width:400, height:250] and Body = https://yada.io/xy.jpg.


### Examples

```
> run (expression 1 2) "This is a test."
  Ok (Text ("This is a test.") 
       (Just { blockOffset = 2, content = "This is a test.", generation = 1, length = 15, offset = 0 }))

> run (expression 1 2) "[strong  stuff]"
  Ok (Inline "strong" [] ("stuff") 
       (Just { blockOffset = 2, content = "[strong  stuff]", generation = 1, length = 15, offset = 0 }))

> run (expression 1 2) "[image [height:40,width:100] stuff]"
  Ok (Inline "image" ["height:40","width:100"] ("stuff") ... 

> run (expression 1 2) "[image [height:40, width:100] stuff]"
  Ok (Inline "image" ["height:40","width:100"] ("lots of stuff") ... 
```  
    
## Blocks

Here is a piece of text that parses to a block:

    |theorem 
    There are infinitely many primes [math p \equiv 1 \modulo 4].
    |end

The body of the block, the line "There are ..." is an inline expression.
The body can also be a block, as in the case of the nested blocks below:

    |indent | theorem [title: Pythagoras]
    There are infinitely many primes [math p \equiv 1 \modulo 4].
    |endall


### Grammar

The grammar for blocks is as follows.  

    Block -> "|" Name Args "|end" 
              | "|" Name InlineExpression "|end"
              | "|" Name Args InlineExpression "|end"
    Name -> String
    Args: as above

### Examples    

Here is how one writes a list:

    |numbered-list

    [item eggs]

    [item milk]

    [item bread]

    |end

A table is written like this:

|table
  |row [Hydrogen, H, 1, 1] |
  |row [Helium, He, 2, 4]  |
  [row |Lithium, Li, 3, 5] |
|end

Or like this:

|table
  |format [l, c, r, r]
  |row [Hydrogen, H, 1, 1] |
  |row [Helium, He, 2, 4]  |
  |row |Lithium, Li, 3, 5] |
|end


## Shorthand 

For common constructs, there are also shorthand features
a la markdown. The idea is to make the composition of text easier.  
Thus, one can also say

    |numbered-list

    - eggs

    - milk

    - bread

    |end

For italic text, one can say _italic text_, and for bold text, one can say *bold text*.
For sections, etc., one can say 

|section Introduction
|subsection Examples

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
    | Inline String (List String) String (Maybe SourceMap)
    | Block String (List String) (Maybe Expression) (Maybe SourceMap)
    | List Expression
```

The SourceMap locates the parsed expression in the source text.
We assume that the text is a list of strings that defines
a list of text blocks (not Blocks!).  A text block is either
an ordinary paragraph (non-blank lines bordered by blank lines)
or an outer Block.  Here is an example:

    0 This is a test.
    1 Ho ho ho!
    2
    3 |theorem 
    4 There are [strong many] primes!
    5 |end
    6 
    7 One, two, [italic three!]

In this example there are three blocks, with blockOffsets
0, 3, and 7.  The source map for [strong many] is

    blockOffset = 3
    offset = 18
    length = 13

To arrive at this conclusion, convert the list of 
lines 3, 4, 5 to the string

    foo = "theorem\nThere are [strong many] primes!\n|end"

and find the offset of the substring "[strong many]".
Consequently we have

    String.slice 18 (18 + 13) foo == "[strong many]"

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

The AST should probably be as follows:


```elm
type Expression
    = Text String (Maybe SourceMap)
    | Inline String (List String) Expression (Maybe SourceMap)
    | Block String (List String) (Maybe Expression) (Maybe SourceMap)
    | List Expression
```

or even


```elm
type Expression
    = Text String (Maybe SourceMap)
    | Inline String (List Expression) Expression (Maybe SourceMap)
    | Block String (List Expression) (Maybe Expression) (Maybe SourceMap)
    | List Expression
```

 as in the case of the AST for MiniLaTeX