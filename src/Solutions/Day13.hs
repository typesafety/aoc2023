{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language PackageImports #-}

module Solutions.Day13 where

import MyPrelude

import "base" Data.List qualified as List

import "containers" Data.Sequence qualified as Sequence
import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P

-- * Part 1

solve1 :: Text -> Text
solve1 = showt . sum . fmap patternNote . partialParse inputP

patternNote :: [[Crap]] -> Int
patternNote crap = partialFromJust (horizNote crap <|> vertNote crap)
  where
    vertNote :: [[Crap]] -> Maybe Int
    vertNote =
        fmap (length . fst)
        . findMiddle Empty
        . Sequence.fromList
        . List.transpose

    horizNote :: [[Crap]] -> Maybe Int
    horizNote =
        fmap ((* 100) . length . fst)
        . findMiddle Empty
        . Sequence.fromList

findMiddle :: Seq [Crap] -> Seq [Crap] -> Maybe (Seq [Crap], Seq [Crap])
findMiddle before after = case (before, after) of
    (_ :|> latest, next :<| rest)
        | latest == next
            && isMiddle (Sequence.reverse before) after -> Just (before, after)
        | otherwise -> findMiddle (before |> next) rest
    (_, Empty) -> Nothing
    (Empty, next :<| rest) -> findMiddle (Empty |> next) rest
  where
    isMiddle :: Seq [Crap] -> Seq [Crap] -> Bool
    isMiddle x y = and (Sequence.zipWith (==) x y)

data Crap = Ash | Rock
    deriving (Eq, Show)

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [[[Crap]]]
inputP = P.sepBy1 patternP P.eol
  where
    patternP :: Parser [[Crap]]
    patternP = P.some rowP

    rowP :: Parser [Crap]
    rowP = P.someTill crapP P.eol

    crapP :: Parser Crap
    crapP = P.char '.' $> Ash <|> P.char '#' $> Rock

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
