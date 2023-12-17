# [Advent of Code 2023](https://adventofcode.com/2023) solutions

Build with `ghc >= 9.6.3` and `cabal-install >= 3.10.2.0`.

Solutions are located in
[src/Solutions](https://github.com/typesafety/aoc2023/tree/main/src/Solutions)

* [Day 1](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day01.hs)
* [Day 2](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day02.hs)
* [Day 3](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day03.hs)
* [Day 4](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day04.hs)
* [Day 5](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day05.hs)
* [Day 6](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day06.hs)
* [Day 7](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day07.hs)
* [Day 8](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day08.hs)
* [Day 9](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day09.hs)
* [Day 10](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day10.hs)
* [Day 11](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day11.hs)
* [Day 12](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day12.hs)
* [Day 13](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day13.hs)
* [Day 14](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day14.hs)
* [Day 15](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day15.hs)
* [Day 16](https://github.com/typesafety/aoc2023/tree/main/src/Solutions/Day16.hs)

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

Re-exports from `app/GHCI.hs`, like `S01` will now exported.
