{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language PackageImports #-}

module Solutions.Day15 where

import MyPrelude

import "base" Data.Char (isAscii, ord)

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P

-- * Part 1

solve1 :: Text -> Text
solve1 = showt . foldl' (\acc step -> acc + hash step) 0 . partialParse inputP

hash :: String -> Int
hash = foldl' hashChar 0
  where
    hashChar :: Int -> Char -> Int
    hashChar current = (`rem` 256) . (* 17) . (+ current) . ord

-- * Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [String]
inputP = P.sepBy stepP (P.char ',') <* P.eol

stepP :: Parser String
stepP = P.some (P.satisfy (\c -> isAscii c && c `notElem` ",\n"))

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
