# OCaml Dataflow

## Prerequisites

Install OCaml and Dune.

## Building

Run `dune build` at the root level.

Run

- `dune exec ./bin/main.exe` for main: this runs the fault tolerance demo.
- `dune exec ./examples/wordcount_benchmark.exe` for wordcount benchmark.
- `dune exec ./examples/wordcount.exe` for sequential wordcount.
- `dune exec ./examples/wordcount_parallel.exe` for parallel wordcount.
- `dune exec ./examples/wordcountjson.exe` for parsing wordcount JSON graph.

Run `dune clean` to clean the build artifacts.

## Formatting

Run `dune fmt`.
