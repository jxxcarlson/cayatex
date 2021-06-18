# CaYaTex: Architecture


# A Bottom-Up View

It is a simple matter to use parser combinators to design a parser for CaYaTeX.
What is more difficult is to design a parser that will return a valid AST regardless
of the input text, providing, in the case of errors, useful, readable, messages
in real time that aid the author in correcting the text.

Below we describe how this is done in CaYaTeX.  As we shall see, the system
is built in layers, where the lowest level is module Element, which we now describe.

## Module Element

At the lowest level is module _Element_ where the type of the AST
and a parser for it are defined.

```
parse : Int -> Int -> String -> Result (List ParseError) Element

type Element
  =   Text String (Maybe Metadata)
    | Element String (List String) Element (Maybe Metadata)
    | LX (List Element) (Maybe Metadata)
```


```
> parse 1 2 "strong stuff"
  Ok (Text ("strong stuff")
        (Just { blockOffset = 2
        , generation = 1
        , label = ""
        , length = 12
        , offset = 0 }))
```

The fragment `(Just ... )` is Metadata, in this case mostly information about
the position of the corresponding source text in the full source text.  We abbreviate
this as

```
> parse 1 2 "strong stuff"
  Ok (Text ("strong stuff") META
```

Here is another example:

```
> parse 1 2 "[b strong stuff]"
Ok (Element "b" [] (LX [Text ("strong stuff") META] Nothing) META)
```


When the `parse` function succeeds, it returns only 
the first legal element of the
input string:

```
> parse 1 2 "some [b strong stuff]"
  Ok (Text ("some ") META)#
```

To parse the full string, use `parseList`:


```
> parseList 1 2 "some [b strong stuff]"
  Ok [ Text ("some ") META
     , Element "b" [] (LX [Text ("strong stuff") META] Nothing) META]#
```

Consider now what happens in the case of an error: Consider now what happens in the case of an error:Consider now what happens in the case of an error:

```
> badString = "foo [b bar"
> parseList 1 2 badString
  Err [{ col = 11
   , contextStack = [{ col = 5, context = CElement, row = 1 }]
   , problem = ExpectingRightBracket, row = 1 }]

```

While the error message gives some information about the error,
it is not in a form that is helpful to an author.  In particular, the rendered
output will not display any part of the source text and will, in sum, be
confusing.  We must do better.

## module Driver

The next step in building a robust error-handling parser is to develop
as system in which errors are not only detected, 
but also corrected, so as to produce a valid AST,
with the correction providing the information
needed to render readable error messages.  
 This process is supposed to happen
is such a short time interval that it is imperceptible
to the author.  Error feedback is psychologically
instantaneous.


Error handling as just described is carried out
using the notion of a `TextCursor,` as in
Matt Griffith's [Elm-Markup  language](https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/).
Before entering into the technicalities, let's give an
example:

```
> import Parser.Data as Data
> initialData = Data.init Data.defaultConfig
> import Parser.Driver exposing(parseLoop)

> parseLoop 1 2 initialData badString |> .parsed
  [[Element "highlightRGB" ["255","130","130"] 
    (LX [Text ("missing right bracket in") ... ]
```

The return value for the `parseLoop` function is long,
but its most important property can be read off immediately:
it is a list of `Element` and so is a valid AST value.
Moreover, its first element gives in-place information about 
the error.  The AST renders as 

```
missing right bracket in ⁅b bar⁆
```

with use of color to make the error message
stand out form the regular text and be easily
parsable by the human eye.

Below is the abstract parseLoop function.  It is made concrete
by the value of `Packet Element`, which is a record of 
functions -- a parser, an error handler, etc.  The `Data`
element accumulates information about the parsed text such 
as section numbers, cross references, etc.  This information
is available when the AST is rendered, so that no second pass,
as with LaTeX, is necessary.

The `parseLoop` function acts by 

```
parseLoop : Packet Element -> Int -> Int -> Data -> String -> TextCursor Element
parseLoop packet generation initialLineNumber data str =
    ParserTool.loop 
      (TextCursor.init generation initialLineNumber data str) 
      (nextCursor packet)

```

```
nextCursor : Packet Element -> TextCursor Element -> ParserTool.Step (TextCursor Element) (TextCursor Element)
nextCursor packet tc =
       -- run the parser on the text held by the TextCursor
        case Parser.run (packet.parser tc.generation tc.blockIndex) tc.text of
            Ok expr ->
                let
                    -- find the length of the source of the parsed expr
                    sourceMapLength =
                        packet.getSource expr 
                          |> Maybe.map .length 
                          |> Maybe.withDefault 0

                    parsand =
                        newExpr packet tc expr

                    data =
                        Data.update parsand tc.data
                in
                ParserTool.Loop
                    { tc
                        | count = tc.count + 1
                        , text = String.dropLeft sourceMapLength tc.text
                        , block = tc.block ++ String.left sourceMapLength tc.text
                        , parsand = Just parsand
                        , parsed = Data.labelElement data parsand :: tc.parsed
                        , offset = tc.offset + sourceMapLength
                        , data = data
                    }

            Err e ->
                case packet.handleError of
                    Nothing ->
                        ParserTool.Loop { tc | count = tc.count + 1 }

                    Just he ->
                        -- Continue loop with the text cursor that the error handler returns
                        ParserTool.Loop (he tc e)
```