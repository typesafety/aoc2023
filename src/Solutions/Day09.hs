{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Solutions.Day09 where

import MyPrelude

import "base" Data.List qualified as List

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "megaparsec" Text.Megaparsec.Char.Lexer qualified as Lex

-- * Part 1

solve1 :: Text -> Text
solve1 = showt . sum . fmap extrapolate . partialParse inputP

-- | Return the next number in the sequence.
extrapolate :: [Int] -> Int
extrapolate ns
    | all (== 0) ns = 0
    | otherwise = partialLast ns + extrapolate (deltas ns)

deltas :: [Int] -> [Int]
deltas = \case
    (x : y : xs) -> List.zipWith (-) (y : xs) (x : y : xs)
    _ -> error "Cannot calculate on lists of length < 2."

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [[Int]]
inputP = P.someTill historyP P.eof

historyP :: Parser [Int]
historyP = P.sepBy (Lex.decimal <|> Lex.signed P.space Lex.decimal) P.hspace1 <* P.eol

-- * Part 2

solve2 :: Text -> Text
solve2 = showt . sum . fmap extrapolate2 . partialParse inputP

extrapolate2 :: [Int] -> Int
extrapolate2 ns
    | all (== 0) ns = 0
    | otherwise = partialHead ns - extrapolate2 (deltas ns)
