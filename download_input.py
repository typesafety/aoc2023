#!/usr/bin/env python3
"""Script for downloading the puzzle input.

Usage:

    ./download_input.py --day=<DAY> --session_id_file=<FILEPATH>

where

    `--day` is the day to download the puzzle input for.
    `--session_id_file` is the path to a file containing the session ID for
        authenticating against https://adventofcode.com.

"""

import argparse
from pathlib import Path
import sys
from urllib.request import Request, urlopen


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='Script for downloading the puzzle input.',
        allow_abbrev=False,
    )
    parser.add_argument(
        '--day',
        type=int,
        choices=range(1, 26),
        required=True
    )
    parser.add_argument(
        '--session_id_file',
        type=str,
        required=True,
        help='Path to file containing the session ID',
    )

    return parser.parse_args()


def download_puzzle_input(day: int, session_id_file_path: Path) -> str:
    with open(session_id_file_path) as f:
        session_id = f.read().strip()
    req = Request(
        f'https://adventofcode.com/2022/day/{day}/input',
        headers={'Cookie': f'session={session_id}'},
    )
    with urlopen(req) as resp:
        puzzle_input = resp.read().decode('utf-8')

    return puzzle_input


def get_puzzle_input_path(day: int) -> Path:
    script_location = Path(__file__).parent.resolve()
    return script_location / 'inputs' / f'{day}.txt'


def main():
    args = parse_args()

    print(f'Day: {args.day}')
    print(f'.session file: {args.session_id_file}')

    puzzle_input_path = get_puzzle_input_path(args.day)
    if puzzle_input_path.exists():
        print(f'Puzzle input for day {args.day} already exists at {puzzle_input_path}')
        sys.exit(1)

    puzzle_input = download_puzzle_input(args.day, Path(args.session_id_file))

    with open(puzzle_input_path, 'w') as f:
        f.write(puzzle_input)

    print('Done.')


if __name__ == '__main__':
    main()
