module Parser.Sections exposing (splitIntoSections)

import Parser.Advanced as Parser

type alias Document =
    { prelude : Lines
    , sections : List Section
    }

type alias Lines = List String 

type alias Section = List String


testStr = """
one
two
three

# Intro
four
five

six
seven

## Body
eight
nine
"""

type LineType = Marked | UnMarked

type ProcessStatus = InPrelude | InSection 

splitIntoSections : String -> { prelude: Lines, sections : List Section}
splitIntoSections str = 
   loop (initialSplliterState str) nextSplitterState

type alias State = { prelude : Lines, sections: List Section , accum: Lines, lines : Lines, status : ProcessStatus }

initialSplliterState : String -> State 
initialSplliterState str = 
  { prelude  = [], sections  = [] , accum = [], lines = String.lines str, status =  InPrelude }   
  
nextSplitterState : State -> Step State Document 
nextSplitterState state = 
  case List.head state.lines of 
    Nothing ->  Done {prelude = List.reverse state.prelude, sections =  List.reverse <| (List.reverse state.accum)::state.sections}
    Just currentLine -> 
      case (lineType currentLine , state.status) of
        (UnMarked, InPrelude) -> 
          -- continue prelude
          Loop { state | lines = List.drop 1 state.lines, accum = currentLine :: state.accum}
        (Marked, InPrelude) -> 
          -- start section
          Loop { state | lines = List.drop 1 state.lines, accum = [currentLine], prelude = state.accum, status = InSection }     
        (UnMarked, InSection) -> 
          -- contunue section                                                                                                                    
          Loop { state | lines = List.drop 1 state.lines, accum = currentLine :: state.accum }     
        (Marked, InSection) -> 
          -- start section
          Loop { state | lines = List.drop 1 state.lines, accum = [currentLine], sections = (List.reverse state.accum) :: state.sections }  
        


lineType : String -> LineType
lineType str =
  if String.left 1 str == "#" then  Marked else UnMarked

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
