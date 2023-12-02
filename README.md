# Advent of Code 2023 solutions

Build with `ghc >= 9.6.3` and `cabal-install >= 3.10.2.0`.

Solutions are located in
[src/Solutions](https://github.com/typesafety/aoc2023/tree/main/src/Solutions)

## Usage

Usage:
```
aoc2023 --day={1-25} --part={1,2} --inputs_dir=<PATH_TO_PUZZLE_INPUTS>
```

* `--day`: `1`-`25`
* `--part`: `1` or `2`
* `--inputs_dir`: Path to a directory containing puzzle inputs.  Puzzle inputs
    should be named `<DAY>.txt` without any leading zeroes.

Example:
```
aoc2023 --day=4 --part=1 --inputs_dir=./inputs
```

## REPL

To launch a REPL with all solution modules loaded (imported qualified as
`S<DAY>`), run the `repl` script (requires `expect` to be installed):

```
./repl
...
aoc>
```

Re-exports from `apps/GHCI.hs`, like `S01` will now exported.
