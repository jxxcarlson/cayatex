module TestRunner exposing (..)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import CayatexBenchmark


main : BenchmarkProgram
main =
    program CayatexBenchmark.suite
