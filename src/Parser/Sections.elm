module Parser.Sections exposing (..)

import Parser.Advanced as Parser


type Label = UnNamed | Named String

type alias Lines = List String 

type alias Section =
    {  label : Label
    , contents : Lines
    }

splitIntoSections : String -> { prelude: Lines, sections : List Section}
splitIntoSections str = 
   let

     lines = String.lines str

     (prelude, rest) =  getSectionContent Nothing lines
        
   in
  { prelude = [], sections = []}


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b
