{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Solutions.Day10 where

import MyPrelude

import "base" Data.List qualified as List

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "unordered-containers" Data.HashMap.Strict qualified as HM

-- * Part 1

solve1 :: Text -> Text
solve1 = showt
    . (`div` 2)
    . List.length
    . (\pm -> path (sLocation pm) (replaceS pm))
    . parsePipeMap
    . partialParse inputP

replaceS :: PipeMap -> PipeMap
replaceS pm = HM.insert (sLocation pm) (sPipe (sLocation pm) pm) pm

sLocation :: PipeMap -> Point
sLocation = fst . partialFromJust . find ((== 'S') . snd) . HM.toList

path :: Point -> PipeMap -> [Point]
path start = go (-1, -1) start
  where
    go :: Point -> Point -> PipeMap -> [Point]
    go prev current pipeMap =
        let next = step prev current pipeMap
        in if next == start
            then [next]
            else next : go current next pipeMap

step :: Point -> Point -> PipeMap -> Point
step prev (x, y) pipeMap = partialHead . List.filter (/= prev) $ case pipeMap HM.! (x, y) of
    '|' -> [(x, y - 1), (x, y + 1)]
    '-' -> [(x - 1, y), (x + 1, y)]
    'L' -> [(x + 1, y), (x, y - 1)]
    'J' -> [(x - 1, y), (x, y - 1)]
    '7' -> [(x - 1, y), (x, y + 1)]
    'F' -> [(x + 1, y), (x, y + 1)]
    c -> error ("Non-pipe symbol: " <> [c])

sPipe :: Point -> PipeMap -> Char
sPipe (x, y) pipeMap = partialFromJust (find @[] predForPipe "|-LJ7F")
  where
    predForPipe :: Char -> Bool
    predForPipe c = all @[] @(Char, [Char]) (uncurry elem) $ case c of
        '|' -> [(u, "|7F"), (d, "|LJ")]
        '-' -> [(l, "-LF"), (r, "-J7")]
        'L' -> [(u, "|7F"), (r, "-J7")]
        'J' -> [(u, "|7F"), (l, "-LF")]
        '7' -> [(d, "|LJ"), (l, "-LF")]
        'F' -> [(d, "|LJ"), (r, "-J7")]
        _ -> error "boo"

    u, d, l, r :: Char
    u = pipeMap HM.! (x, y - 1)
    d = pipeMap HM.! (x, y + 1)
    l = pipeMap HM.! (x - 1, y)
    r = pipeMap HM.! (x + 1, y)

-- | Coordinate, (0, 0) being top left.
type Point = (Int, Int)

type PipeMap = HashMap Point Char

parsePipeMap :: [[Char]] -> PipeMap
parsePipeMap = foldl' HM.union HM.empty . List.zipWith parseLine [0 ..]
  where
    parseLine :: Int -> [Char] -> PipeMap
    parseLine y = HM.fromList . List.zipWith (\x char -> ((x, y), char)) [0 ..]

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [[Char]]
inputP = P.someTill (P.someTill P.asciiChar P.eol) P.eof

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
