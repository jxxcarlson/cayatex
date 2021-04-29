# Notes

## Architecture

- Parser.Element
- Parser.Driver
- Parser.Document


## Parser.Element

Parser.Element declares `Element`, the type of the AST, and provides 
a function `parser` which is used to parse strings to elements. 
The `SourceMap` of an element provides the location in the source 
text of the part of the text parsed into the given element.

```elm
type Element
    = Text String (Maybe SourceMap)
    | Element String (List String) Element (Maybe SourceMap)
    | LX (List Element) (Maybe SourceMap)
```




```elm
parser : Int -> Int -> Parser Element
``` 

where the first two arguments are the `generation` and `lineNumber`.

### Parser.SourceMap

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

### Examples

Using `run` from `Parser.Advanced` and `strip` from `Parser.Getters`, 
we  have

```elm
> run (parser 0 0) "This [i is] a test."
Ok (Text ("This ") (Just { blockOffset = 0, generation = 0, length = 5, offset = 0 }))
```

```elm
> run (listParser 0 0) "This [i is] a test." |> Result.map (List.map strip)
Ok [ Text ("This ") Nothing
   , Element "i" [] (LX [Text "is" Nothing] Nothing) Nothing
   , Text (" a test.") Nothing]
 ```

## Parser.TextCursor and Parser.Loop

The aim of `TextCursor`  and `Loop` is to provide robust error 
recovery as `parser` is repeatedly applied to a string of source text.


```elm
type alias TextCursor a =
    { text : String          -- text remaining to be parsed
    , block : String         -- 
    , parsed : List a        -- cumulative parse result
    , stack : List String    -- for error recovery
    , blockIndex : Int       -- for SourceMap
    , offset : Int           -- for SourceMap
    , generation : Int       -- See XXX
    , count : Int            -- "program counter"
    , data : Data            -- data accumulated for rendering
    }
```


```elm
parseLoop : Packet a -> Int -> Int -> String -> TextCursor a
```



```elm
case Parser.run (packet.parser tc.generation tc.blockIndex) tc.text of
    Ok expr ->
        let
            sourceMapLength =
                packet.getSource expr |> Maybe.map .length |> Maybe.withDefault 0
    
        in
        ParserTool.Loop
            { tc
                | count = tc.count + 1
                , text = String.dropLeft sourceMapLength tc.text
                , block = tc.block ++ String.left sourceMapLength tc.text
                , parsed = newExpr packet tc expr :: tc.parsed
                , offset = tc.offset + sourceMapLength
                , data = packet.updateData expr tc.data
            }
    Err e ->
        case packet.handleError of
            Nothing ->
                ParserTool.Loop { tc | count = tc.count + 1 }

            Just he ->
                ParserTool.Loop (he tc e)                    
```

Function `parseLoop` proceeds as follows.  To begin, it applies 
`packet.parser` to `tc.text`.  Assuming a successful result,
the TextCursor is updated as follows and parseLoop runs again
with this new value.

- increments the program counter
- drops the part of `text` just parsed and stores the result in `text`.
- postpends the dropped text to `block`(_what is this used for?_)
- updates the offset
- updates the `data` field



In the case of an unsuccesful parse and an installed  error handler,
the error handler is called won the current text cursor and the error
value to produce a new value of the text cursor.  Parseloop runs again
with the new value of TextCursor.



```elm
handleError : TextCursor Element -> List ErrorData -> TextCursor Element
```