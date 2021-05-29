module Parser.Document exposing (..)

import Parser.Lines as Lines
import Parser.Types exposing (..)


process : Int -> Document -> { processedPrelude : State, processedSections : List State }
process generation { prelude, sections } =
    { processedPrelude = Lines.process generation prelude, processedSections = List.map (Lines.process generation) sections }
